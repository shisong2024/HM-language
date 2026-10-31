{-# LANGUAGE LambdaCase, FlexibleContexts, OverloadedStrings #-}
module Interp.TypeCheck.Declarations where

import Interp.Types

import Control.Monad (forM_, unless)
import Control.Monad.Except (MonadError (throwError))

import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Reader (MonadReader (ask))
import Interp.Parser (dataDeclsOf)
import Interp.TypeCheck.Common

typeCtorArity :: DEnv -> Text -> Maybe Int
typeCtorArity denv n = case M.lookup n (denvDatas denv) of
    Just (c: _) -> case M.lookup c (denvCtors denv) of
        Just ci -> case ciRes ci of
            TCon _ as -> Just (length as)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

checkTypeCons :: MonadError (Located TypeError) m => DEnv -> T' -> m ()
checkTypeCons denv = \case
    TInt -> return ()
    TBool -> return ()
    TString -> return ()
    TVar _ -> return ()
    TList t -> checkTypeCons denv t
    TTuple ts -> mapM_ (checkTypeCons denv) ts
    TFunc a e b -> checkEffRow denv e >> checkTypeCons denv a >> checkTypeCons denv b
    TCon n as -> do
        case M.lookup n (denvDatas denv) of
            Nothing -> throwError $ Located Nothing $ UnknownTypeCtor n
            Just _ -> case typeCtorArity denv n of
                Just k | k /= length as -> throwError $ Located Nothing $ TypeCtorArityMisMatch n k (length as)
                _ -> return ()
        mapM_ (checkTypeCons denv) as

checkAbilityType :: MonadError (Located TypeError) m => DEnv -> T' -> m ()
checkAbilityType denv ty = do
    checkTypeCons denv ty
    case ty of
        TCon n _ | S.member n (denvAbilities denv) -> return ()
        _ -> throwError $ Located Nothing $ NotAnAbility ty

checkEffRow :: MonadError (Located TypeError) m => DEnv -> Eff -> m ()
checkEffRow denv e = forM_ (S.toList (effLabs e)) $ \n -> do
    unless (S.member n (denvEffs denv)) $ throwError $ Located Nothing $ UnknownEff n
    let expected = maybe 0 (length . edParams) (M.lookup n (denvOperations denv))
        args = M.findWithDefault [[]] n (effParams e)
    forM_ args $ \ts -> do
        unless (length ts == expected) $ throwError $ Located Nothing $ HandleError ("effect arity mismatch: " <> n)
        mapM_ (checkTypeCons denv) ts

checkEffTypes :: MonadError (Located TypeError) m => DEnv -> T' -> m ()
checkEffTypes denv = \case
    TFunc a e b -> checkEffRow denv e >> checkEffTypes denv a >> checkEffTypes denv b
    TList a -> checkEffTypes denv a
    TTuple as -> mapM_ (checkEffTypes denv) as
    TCon _ as -> mapM_ (checkEffTypes denv) as
    _ -> return ()

buildDEnv :: [DataDecl] -> DEnv
buildDEnv ds = DEnv
    { denvCtors = M.fromList [(c, ctorInfo d args) | d <- ds, (c, args) <- dCtors d]
    , denvDatas = M.fromList [(dName d, map fst (dCtors d)) | d <- ds]
    , denvAbilities = S.empty
    , denvEffs = S.fromList ["Console", "FileIO"]
    , denvOperations = M.empty
    , denvFields = M.fromList [(dName d, dFields d) | d <- ds]
    }

buildProgramDEnv :: Program -> DEnv
buildProgramDEnv p = (buildDEnv (dataDeclsOf p))
    { denvAbilities = S.fromList [dName d | StmtAbility d <- p]
    , denvEffs = S.insert "Console" (S.fromList $ [n | StmtEff n <- p] <> [edName d | StmtOperations d <- p])
    , denvOperations = M.fromList [(edName d, d) | StmtOperations d <- p]
    }

mergeDEnv :: DEnv -> DEnv -> DEnv
mergeDEnv new old =
    let redefined = M.keysSet (denvDatas new)
        stale = concat [cs | (n, cs) <- M.toList (denvDatas old), S.member n redefined]
    in DEnv
        { denvCtors = M.union (denvCtors new) (foldr M.delete (denvCtors old) stale)
        , denvDatas = M.union (denvDatas new) (denvDatas old)
        , denvAbilities = S.union (denvAbilities new) (denvAbilities old `S.difference` redefined)
        , denvEffs = S.union (denvEffs new) (denvEffs old)
        , denvOperations = M.union (denvOperations new) (denvOperations old)
        , denvFields = M.union (denvFields new) (denvFields old)
        }

ctorInfo :: DataDecl -> [T'] -> CtorInfo
ctorInfo d args = let tvs = S.fromList [0..length (dParams d) - 1] in CtorInfo
    { ciTvs  = tvs
    , ciRes  = TCon (dName d) [TVar i | i <- S.toList tvs]
    , ciArgs = args
    }

ctorSchemes :: DataDecl -> TEnv
ctorSchemes d = M.fromList
    [ (c, Forall (ciTvs ci) [] (foldr pureFunc (ciRes ci) (ciArgs ci)))
    | (c, args) <- dCtors d, let ci = ctorInfo d args]

checkDataDecls :: (MonadReader DEnv m, MonadError (Located TypeError) m) => [DataDecl] -> m ()
checkDataDecls ds = do
    denv <- ask
    mapM_ (checkTypeCons denv) [at | d <- ds, (_, args) <- dCtors d, at <- args]
    case fstDuplicate (map dName ds) of
        Just n -> throwError $ Located Nothing $ DuplicateData n
        Nothing -> return ()
    case fstDuplicate (concatMap (map fst . dCtors) ds) of
        Just c -> throwError $ Located Nothing $ DuplicateCtor c
        Nothing -> return ()
    let redef = map dName ds
        live  = [c | (n, cs) <- M.toList (denvDatas denv), n `notElem` redef, c <- cs]
    case [c | c <- concatMap (map fst . dCtors) ds, c `elem` live] of
        (c: _) -> throwError $ Located Nothing $ DuplicateCtor c
        [] -> return ()

-- ==== 字段多态：合成的 getter / setter ability ====
--
-- 字段约束需要在类型层区分「哪个字段」，而语言没有类型级字符串。方案：为每个
-- 字段名 x 合成两个 ability —— `$get_x r a`（投影）和 `$set_x r a`（更新），
-- 字段名编码进 ability 名字里，于是不同字段的约束类型头不同，天然不歧义。
-- 这些 ability 只作为约束的类型头存在（让 checkAbilityType 通过）；真正的字典
-- （投影/更新函数）在 Infer.resolveFieldWanted 里按具体 data 现场构造并拼接。

getterAbility, setterAbility :: Text -> Text
getterAbility x = "$get_" <> x
setterAbility x = "$set_" <> x

-- | 从一个约束类型头名反解出字段名（若是合成字段 ability）。
getterFieldOf, setterFieldOf :: Text -> Maybe Text
getterFieldOf n = T.stripPrefix "$get_" n
setterFieldOf n = T.stripPrefix "$set_" n

isFieldAbilityName :: Text -> Bool
isFieldAbilityName n = "$get_" `T.isPrefixOf` n || "$set_" `T.isPrefixOf` n

-- | 为给定字段名集合合成 getter/setter ability 的 DEnv 片段。
-- 形状：`$get_x r a = $get_x { fldGet :: r -> a }`、`$set_x r a = $set_x { fldSet :: a -> r -> r }`。
buildFieldAbilities :: [Text] -> DEnv
buildFieldAbilities xs = (buildDEnv decls)
    { denvAbilities = S.fromList (map getterAbility xs <> map setterAbility xs) }
  where
    decls = concat [[getterDecl x, setterDecl x] | x <- xs]
    getterDecl x = DataDecl (getterAbility x) ["r", "a"]
        [(getterAbility x, [TFunc (TVar 0) emptyEff (TVar 1)])]
        (M.fromList [("fldGet", (getterAbility x, 0))])
    setterDecl x = DataDecl (setterAbility x) ["r", "a"]
        [(setterAbility x, [TFunc (TVar 1) emptyEff (TFunc (TVar 0) emptyEff (TVar 0))])]
        (M.fromList [("fldSet", (setterAbility x, 0))])
