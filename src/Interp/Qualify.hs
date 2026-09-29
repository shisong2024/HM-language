{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Interp.Qualify where

import Interp.Types
import Interp.Eval

import qualified Data.Set as S

import Data.Text (Text)

topsOf :: Program -> Tops
topsOf prs = Tops
    { tVals  = S.fromList [n | StmtDef (Def n _ _ ) <- prs]
    , tTypes = S.fromList ([dName d | StmtData d <- prs] ++ [n | StmtType n _ _ <- prs])
    , tCtors = S.fromList [c | StmtData d <- prs, (c, _) <- dCtors d]
    }

qualifyProgram :: Text -> Program -> Program
qualifyProgram a prs = map (stmt (topsOf prs)) prs
    where
        qual :: S.Set Text -> Text -> Text
        qual s n = if n `S.member` s then a <> "." <> n else n

        stmt :: Tops -> Statement -> Statement
        stmt t = \case
            StmtDef (Def n vs b) -> StmtDef (Def (qual (tVals t) n) vs (expr t S.empty b))
            StmtExpr e -> StmtExpr (expr t S.empty e)
            StmtData d -> StmtData d
                { dName  = qual (tTypes t) (dName d)
                , dCtors = [(qual (tCtors t) c, map (typ t) as) | (c, as) <- dCtors d]
                }
            StmtType n ps ty -> StmtType (qual (tTypes t) n) ps (typ t ty)
        
        expr :: Tops -> S.Set Text -> E' -> E'
        expr t bs = \case
            Var n -> Var (if n `S.member` bs then n else qual (S.union (tVals t) (tCtors t)) n)
            ILit i -> ILit i
            BLit b -> BLit b
            ListLit es -> ListLit (map (expr t bs) es)
            TupleLit es -> TupleLit (map (expr t bs) es)
            Let n u v -> Let n (expr t bs u) (expr t (S.insert n bs) v)
            If b u v -> If (expr t bs b) (expr t bs u) (expr t bs v)
            BOpr o u v -> BOpr o (expr t bs u) (expr t bs v)
            Lambda n b -> Lambda n (expr t (S.insert n bs) b)
            App u v -> App (expr t bs u) (expr t bs v)
            Match s as -> Match (expr t bs s) 
                [(pat t p, expr t (S.union bs (S.fromList (patVarsList p))) b) | (p, b) <- as]
            AnnT e ty -> AnnT (expr t bs e) (typ t ty)
            At sp e -> At sp (expr t bs e)
        
        pat :: Tops -> P' -> P'
        pat t = \case
            PCtor n ps -> PCtor (qual (tCtors t) n) (map (pat t) ps)
            PCons u v  -> PCons (pat t u) (pat t v)
            PTuple ps  -> PTuple (map (pat t) ps)
            PVar n     -> PVar n
            PWild      -> PWild
            PInt n     -> PInt n
            PBool b    -> PBool b
            PNil       -> PNil

        typ :: Tops -> T' -> T'
        typ t = \case
            TCon n ts -> TCon (qual (tTypes t) n) (map (typ t) ts)
            TList u   -> TList (typ t u)
            TTuple ts -> TTuple (map (typ t) ts)
            TFunc u v -> TFunc (typ t u) (typ t v)
            TInt      -> TInt
            TBool     -> TBool
            TVar i    -> TVar i
