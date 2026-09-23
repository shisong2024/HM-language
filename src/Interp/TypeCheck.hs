{-# LANGUAGE LambdaCase, FlexibleContexts, TupleSections #-}
module Interp.TypeCheck where

import Interp.Types
import Data.Map ((!?))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.Error.Class (MonadError (throwError, catchError))
import Control.Monad.State (MonadState, get, put)
import Interp.Eval
import Data.Graph (stronglyConnComp, SCC)
import Control.Monad (foldM, forM)
import Interp.Pretty (spanStart)

typeChecker :: (MonadState Counter m, MonadError (Located TypeError) m) => TEnv -> E' -> m (TSub, T')
typeChecker env = \case
    ILit _ -> return (M.empty, TInt)

    BLit _ -> return (M.empty, TBool)

    Var t -> case env !? t of
        Nothing -> throwError (Located Nothing $ UnboundVar t)
        Just s  -> do
            tp <- instantiate s
            return (M.empty, tp)
    
    BOpr bopr e1 e2 -> do
        (ts1, tp1) <- typeChecker env e1
        (ts2, tp2) <- typeChecker (applyTEnv ts1 env) e2
        ts3 <- unify (apply ts2 tp1, TInt)
        ts4 <- unify (apply ts3 tp2, TInt)
        return 
            ( compose ts4 $ compose ts3 $ compose ts2 ts1
            , case bopr of
                OArith _ -> TInt
                OCmp _ -> TBool
            )
     
    Let t e1 e2 -> do
        tv <- fresh
        (ts1, tp1) <- typeChecker (M.insert t (Forall S.empty tv) env) e1
        ts2 <- unify (apply ts1 tv, tp1)
        let ts' = compose ts2 ts1
            nenv = applyTEnv ts' env
            s = generalize nenv (apply ts' tp1)
        (ts3, tp2) <- typeChecker (M.insert t s nenv) e2
        return (compose ts3 ts', tp2)

    If eb e1 e2 -> do
        (ts1, tpb) <- typeChecker env eb
        ts2 <- unify (apply ts1 tpb, TBool)
        let env' = applyTEnv (compose ts2 ts1) env
        (ts3, tp1) <- typeChecker env' e1
        let ts' = compose ts3 (compose ts2 ts1)
        (ts4, tp2) <- typeChecker (applyTEnv ts' env') e2
        ts5 <- unify (apply ts4 tp1, apply ts4 tp2)
        return (compose ts5 $ compose ts4 ts', apply ts5 tp1)

    Lambda t e -> do
        tv <- fresh
        (ts, btp) <- typeChecker (M.insert t (Forall S.empty tv) env) e
        return (ts, apply ts (TFunc tv btp))

    App f arg -> do
        (ts1, ft) <- typeChecker env f
        (ts2, argt) <- typeChecker (applyTEnv ts1 env) arg
        rt <- fresh
        ts3 <- unify (apply ts2 ft, TFunc argt rt) 
        return (compose ts3 $ compose ts2 ts1, apply ts3 rt)

    At sp e -> catchError (typeChecker env e) (throwError . addSpan sp)

sccChecker :: (MonadState Counter m, MonadError (Located TypeError) m) => TEnv -> [Binding] -> m (TSub, TEnv)
sccChecker env bs = do
    tvs <- mapM (const fresh) bs
    let ns   = map fst bs
        genv = M.union (M.fromList (zip ns (map (Forall S.empty) tvs))) env
        bds  = map snd bs
    res <- mapM (typeChecker genv) bds
    let tps = map snd res
        ts0 = foldr (compose .fst) M.empty res
    ts <- foldM 
        (\acc (tv, tp) -> do
            d <- unify (apply acc tv, apply acc tp)
            return $ compose d acc
        ) ts0 (zip tvs tps)
    let env' = applyTEnv ts env
        sms  = [(n, generalize env' (apply ts tp)) | (n, tp) <- zip ns tps]
    return (ts, M.fromList sms)

programChecker :: (MonadState Counter m, MonadError (Located TypeError) m) => TEnv -> Program -> m (TSub, TEnv, M.Map Int StmtTy)
programChecker outer prs = do
    (ts, nTEnv) <- go M.empty outer (stronglyConnComp $ deps prs)
    let env' = applyTEnv ts (M.union nTEnv outer)
    exprs <- forM [(i, e) | (i, StmtExpr e) <- zip [0..] prs] $ \(i, e) -> (i, ) <$> tryEnv (fmap (apply ts . snd) (typeChecker env' e))
    let defTps = M.fromList [(i, TyDef n (nTEnv M.! n)) | (i, StmtDef (Def n _ _)) <- zip [0..] prs]
    return (ts, nTEnv, M.union defTps (M.fromList (map (fmap TyExpr) exprs)))

    where
        go :: (MonadState Counter m, MonadError (Located TypeError) m) => TSub -> TEnv -> [SCC Decl] -> m (TSub, TEnv)
        go ts env = \case
            [] -> return (ts, env)
            (scc: rest) -> do
                let grp = [(n, b) | Def n _ b <- flatten scc]
                (ts1, new) <- sccChecker env grp
                go (compose ts1 ts) (M.union new env) rest

fresh :: MonadState Counter m => m T'
fresh = get >>= \n -> put (n + 1) >> return (TVar n)

apply :: TSub -> T' -> T'
apply ts = \case
    TVar i -> case ts !? i of
        Nothing -> TVar i
        Just tp -> tp
    TFunc argt rest -> TFunc (apply ts argt) (apply ts rest)
    t -> t

applyS' :: TSub -> S' -> S'
applyS' ts (Forall tvs t) = Forall tvs (apply (foldr M.delete ts tvs) t)

applyTEnv :: TSub -> TEnv -> TEnv
applyTEnv = M.map . applyS'

compose :: TSub -> TSub -> TSub
compose s2 s1 = M.union (M.map (apply s2) s1) s2

unify :: MonadError (Located TypeError) m => (T', T') -> m TSub
unify = \case
    (TInt, TInt) -> return M.empty
    (TBool, TBool) -> return M.empty
    (TVar i, tp)  -> bindVar i tp
    (tp, TVar i)  -> bindVar i tp
    (TFunc a1 r1, TFunc a2 r2) -> do
        ts  <- unify (a1, a2)
        ts' <- unify (apply ts r1, apply ts r2)
        return $ compose ts ts'
    (t1, t2) -> throwError (Located Nothing $ TypeMismatch t1 t2)

bindVar :: MonadError (Located TypeError) m => TypeVar -> T' -> m TSub
bindVar i (TVar j)
    | i == j = return M.empty
bindVar i t = if i `occursIn` t then throwError $ Located Nothing OccurCheck else return (M.singleton i t)

instantiate :: MonadState Counter m => S' -> m T'
instantiate (Forall tvs tp) = do
    ftvs <- mapM (const fresh) (S.toList tvs)
    return (apply (M.fromList $ zip (S.toList tvs) ftvs) tp)

occursIn :: TypeVar -> T' -> Bool
occursIn i = \case
    TVar j -> i == j
    TFunc at rt -> i `occursIn` at || i `occursIn` rt
    _ -> False

generalize :: TEnv -> T' -> S'
generalize env t = Forall (ftv t S.\\ ftvTEnv env) t

ftv :: T' -> Set TypeVar
ftv = \case
    TVar i -> S.singleton i
    TFunc arg res -> S.union (ftv arg) (ftv res)
    _ -> S.empty

ftvS' :: S' -> Set TypeVar
ftvS' (Forall tvs tp) = ftv tp `S.difference` tvs

ftvTEnv :: TEnv -> Set TypeVar
ftvTEnv = S.unions . map ftvS' . M.elems

errLine :: Located TypeError -> Int
errLine = \case
    Located (Just sp) _ -> fst (spanStart sp)
    _ -> 1