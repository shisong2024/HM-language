{-# LANGUAGE LambdaCase, TupleSections, FlexibleContexts #-}
module Interp.TypeCheck.Pattern where

import Interp.Eval (patVarsList)
import Interp.Types

import Control.Monad.Except (MonadError (throwError))
import Control.Monad (foldM)
import Control.Monad.Reader (ask)

import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Map ((!?))

import qualified Data.Map as M
import qualified Data.Set as S

import Interp.TypeCheck.Common
import Interp.TypeCheck.Solver


patType :: Checker m => P' -> m (TSub, T', TEnv)
patType = \case
    PWild -> fresh <&> (M.empty, , M.empty)
    PVar x -> fresh >>= \v -> return (M.empty, v, M.singleton x (Forall S.empty [] v))
    PInt _ -> return (M.empty, TInt, M.empty)
    PStr _ -> return (M.empty, TString, M.empty)
    PBool _ -> return (M.empty, TBool, M.empty)
    PNil -> fresh <&> ((M.empty, , M.empty) . TList)

    PCons ph pt -> do
        (ts1, th, env1) <- patType ph
        (ts2, tt, env2) <- patType pt
        let tsA = compose ts2 ts1
        v <- fresh
        ts3 <- unify (apply tsA th, apply tsA v)
        let tsB = compose ts3 tsA
        ts4 <- unify (apply tsB tt, apply tsB (TList v))
        let tsf = compose ts4 tsB
        return (tsf, apply tsf (TList v), M.union (applyTEnv tsf env1) (applyTEnv tsf env2))

    PTuple ps -> do
        (ts, tps, tenv) <- foldM step (M.empty, [], M.empty) ps
        return (ts, TTuple tps, tenv)

    PCtor n ps -> do
        denv <- ask
        case denvCtors denv !? n of
            Nothing -> throwError $ Located Nothing $ UnknownCtor n
            Just ci -> do
                ftvs <- mapM (const fresh) (S.toList (ciTvs ci))
                let sub   = M.fromList (zip (S.toList (ciTvs ci)) ftvs)
                    resT  = apply sub (ciRes ci)
                    argTs = fmap (apply sub) (ciArgs ci)
                if length ps /= length argTs
                then throwError $ Located Nothing $ CtorArityMisMatch n (length argTs) (length ps)
                else do
                    (ts, tenv) <- foldM step' (M.empty, M.empty) (zip ps argTs)
                    return (ts, apply ts resT, tenv)
    where
        step :: Checker m => (TSub, [T'], TEnv) -> P' -> m (TSub, [T'], TEnv)
        step (ts, tps, tenv) p = do
            (ts1, tp1, tenv1) <- patType p
            let ts' = compose ts1 ts
            return (ts', tps ++ [apply ts' tp1], M.union (applyTEnv ts' tenv1) tenv)

        step' :: Checker m => (TSub, TEnv) -> (P', T') -> m (TSub, TEnv)
        step' (ts, tenv) (p, argT) = do
            (ts1, tp1, tenv1) <- patType p
            let ts' = compose ts1 ts
            ts2 <- unify (apply ts' tp1, apply ts' argT)
            let ts'' = compose ts2 ts'
            return (ts'', M.union (applyTEnv ts'' tenv1) (applyTEnv ts'' tenv))

checkDupPatVar :: MonadError (Located TypeError) m => P' -> m ()
checkDupPatVar p = case fstDuplicate (patVarsList p) of
    Just x -> throwError $ Located Nothing $ DuplicatePatVar x
    Nothing -> return ()

irrefutable :: P' -> Bool
irrefutable = \case
    PVar _    -> True
    PWild     -> True
    PTuple ps -> all irrefutable ps
    PCtor _ _ -> False
    _         -> False


exhaustive :: DEnv -> T' -> [P'] -> Bool
exhaustive denv t ps = any irrefutable ps || case t of
    TInt -> True
    TString -> True
    TVar _ -> True
    TFunc {} -> True
    TList te ->
        let consRows = ([[h, tt] | PCons h tt <- ps])
        in  any (\case PNil -> True; _ -> False) ps
        && (any (\case PCons h tt -> irrefutable h && irrefutable tt ; _ -> False) ps
        || (not (null consRows)
        && exhaustive denv te [h | [h, _] <- consRows]
        && exhaustive denv (TList te) [tt | [_, tt] <- consRows]))
    TBool ->
           any (\case PBool True -> True; _ -> False) ps
        && any (\case PBool False -> True; _ -> False) ps
    TTuple ts ->
        let col j = [qs !! j | PTuple qs <- ps, length qs == length ts] in
        any (\case
            PTuple qs -> length ts == length qs && all irrefutable qs
            _ -> False
        ) ps || and [exhaustive denv tj (col j) | (j, tj) <- zip [0..] ts]
    TCon n act -> case denvDatas denv !? n of
        Nothing -> True
        Just ctors -> all (covered act) ctors
    where
        covered :: [T'] -> Text -> Bool
        covered act c =
            any (\case PCtor m qs -> m == c && all irrefutable qs; _ -> False) ps ||
            case denvCtors denv !? c of
                Nothing -> False
                Just ci ->
                    let ats  = map (instantiateArgs act) (ciArgs ci)
                        rows = [qs | PCtor m qs <- ps, m == c, length qs == length ats]
                    in  not (null rows)
                    &&  and [exhaustive denv tj [q !! j | q <- rows] | (j, tj) <- zip [0..] ats]

        instantiateArgs :: [T'] -> T' -> T'
        instantiateArgs as= apply (M.fromList [(i, as !! i) | i <- [0..length as - 1]])
