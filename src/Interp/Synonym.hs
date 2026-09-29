{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Interp.Synonym where

import Interp.Types

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Control.Monad (foldM)
import Data.Text (Text)

expandProgram :: SynTable -> Program -> Either Text (SynTable, Program)
expandProgram tab0 prs = do
    tab <- foldM addSyn tab0 prs
    prs' <- mapM (expandStmt tab) prs
    return (tab, prs')
    where
        addSyn :: SynTable -> Statement -> Either Text SynTable
        addSyn tab = \case
            StmtType n ps t -> Right (M.insert n (ps, t) tab)
            _               -> Right tab

expandStmt :: SynTable -> Statement -> Either Text Statement
expandStmt tab = \case
    StmtDef (Def n vs b) -> StmtDef . Def n vs <$> expandE tab b
    StmtExpr e           -> StmtExpr <$> expandE tab e
    StmtData d           -> do
        cs <- mapM (\(c, as) -> (,) c <$> mapM (expandT tab) as) (dCtors d)
        return $ StmtData d { dCtors = cs }
    StmtType n ps t      -> Right (StmtType n ps t)

expandE :: SynTable -> E' -> Either Text E'
expandE tab = \case
    ILit i      -> Right (ILit i)
    BLit b      -> Right (BLit b)
    Var n       -> Right (Var n)
    ListLit es  -> ListLit <$> mapM (expandE tab) es
    TupleLit es -> TupleLit <$> mapM (expandE tab) es
    Let n u v   -> Let n <$> expandE tab u <*> expandE tab v
    If b u v    -> If <$> expandE tab b <*> expandE tab u <*> expandE tab v
    BOpr o u v  -> BOpr o <$> expandE tab u <*> expandE tab v
    Lambda n b  -> Lambda n <$> expandE tab b
    App u v     -> App <$> expandE tab u <*> expandE tab v
    Match s as  -> Match <$> expandE tab s
                         <*> mapM (\(p, b) -> (,) p <$> expandE tab b) as
    AnnT e ty   -> AnnT <$> expandE tab e <*> expandT tab ty
    At sp e     -> At sp <$> expandE tab e

expandT :: SynTable -> T' -> Either Text T'
expandT tab = go S.empty
    where
        go :: S.Set Text -> T' -> Either Text T'
        go seen = \case
            TInt      -> Right TInt
            TBool     -> Right TBool
            TVar i    -> Right (TVar i)
            TList t   -> TList <$> go seen t
            TTuple ts -> TTuple <$> mapM (go seen) ts
            TFunc a b -> TFunc <$> go seen a <*> go seen b
            TCon n args -> case M.lookup n tab of
                Nothing -> TCon n <$> mapM (go seen) args
                Just (ps, rhs)
                    | n `S.member` seen -> Left $
                        "cyclic type synonym: " <> n <> " is defined in terms of itself."
                    | length ps /= length args -> Left $
                        "Type synonym " <> n <> " expects " <> tshow (length ps)
                        <> " argument(s) but got " <> tshow (length args) <> "."
                    | otherwise -> do
                        as <- mapM (go seen) args
                        go (S.insert n seen) (subst (M.fromList (zip [0 :: TypeVar ..] as)) rhs)

subst :: M.Map TypeVar T' -> T' -> T'
subst m = \case
    TInt      -> TInt
    TBool     -> TBool
    TVar i    -> M.findWithDefault (TVar i) i m
    TList t   -> TList (subst m t)
    TTuple ts -> TTuple (map (subst m) ts)
    TFunc a b -> TFunc (subst m a) (subst m b)
    TCon n as -> TCon n (map (subst m) as)

tshow :: Show a => a -> Text
tshow = T.pack . show
