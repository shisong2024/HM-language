{-# LANGUAGE LambdaCase, FlexibleContexts, OverloadedStrings #-}
module Interp.TypeCheck.Declarations where

import Interp.Types

import Control.Monad (forM_, unless)
import Control.Monad.Except (MonadError (throwError))

import Data.Text (Text)

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
checkEffRow denv e = forM_ (S.toList (effLabs e)) $ \n ->
    unless (S.member n (denvEffs denv)) $ throwError $ Located Nothing $ UnknownEff n

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
    , denvEffs = S.singleton "Console"
    }

buildProgramDEnv :: Program -> DEnv
buildProgramDEnv p = (buildDEnv (dataDeclsOf p))
    { denvAbilities = S.fromList [dName d | StmtAbility d <- p]
    , denvEffs = S.insert "Console" (S.fromList [n | StmtEff n <- p])
    }

mergeDEnv :: DEnv -> DEnv -> DEnv
mergeDEnv new old =
    let redefined = M.keysSet (denvDatas new)
        stale = concat [cs | (n, cs) <- M.toList (denvDatas old), S.member n redefined]
    in DEnv
        { denvCtors = M.union (denvCtors new) (foldr M.delete (denvCtors old) stale)
        , denvDatas = M.union (denvDatas new) (denvDatas old)
        , denvAbilities = S.union (denvAbilities new) (denvAbilities old `S.difference` redefined)
        ,  denvEffs = S.union (denvEffs new) (denvEffs old)
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
