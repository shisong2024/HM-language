{-# LANGUAGE LambdaCase, FlexibleContexts, OverloadedStrings #-}
module Interp.TypeCheck where

import Interp.Types
import Data.Map ((!?))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State (MonadState, get, put)

typeChecker :: (MonadState Counter m, MonadError TypeError m) => TEnv -> E' -> m (TSub, T')
typeChecker env = \case
    Lit _ -> return (M.empty, TInt)

    Var t -> case env !? t of
        Nothing -> throwError (UnboundVar t)
        Just s  -> do
            tp <- instantiate s
            return (M.empty, tp)
    
    ArithOpr _ e1 e2 -> do
        (ts1, tp1) <- typeChecker env e1
        (ts2, tp2) <- typeChecker (applyTEnv ts1 env) e2
        ts3 <- unify (apply ts2 tp1, TInt)
        ts4 <- unify (apply ts3 tp2, TInt) 
        return (compose ts4 $ compose ts3 $ compose ts2 ts1, TInt)
     
    Let t e1 e2 -> do
        (ts1, tp1) <- typeChecker env e1
        let nenv = applyTEnv ts1 env
        let s = generalize nenv (apply ts1 tp1)
        (ts2, tp2) <- typeChecker (M.insert t s nenv) e2
        return (compose ts2 ts1, tp2)

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

fresh :: MonadState Counter m => m T'
fresh = get >>= \n -> put (n + 1) >> return (TVar n)

apply :: TSub -> T' -> T'
apply ts = \case
    TInt -> TInt
    TVar i -> case ts !? i of
        Nothing -> TVar i
        Just tp -> tp
    TFunc argt rest -> TFunc (apply ts argt) (apply ts rest)

applyS' :: TSub -> S' -> S'
applyS' ts (Forall tvs t) = Forall tvs (apply (foldr M.delete ts tvs) t)

applyTEnv :: TSub -> TEnv -> TEnv
applyTEnv = M.map . applyS'

compose :: TSub -> TSub -> TSub
compose s2 s1 = M.union (M.map (apply s2) s1) s2

unify :: MonadError TypeError m => (T', T') -> m TSub
unify = \case
    (TInt, TInt) -> return M.empty
    (TVar i, tp)  -> bindVar i tp
    (tp, TVar i)  -> bindVar i tp
    (TFunc a1 r1, TFunc a2 r2) -> do
        ts  <- unify (a1, a2)
        ts' <- unify (apply ts r1, apply ts r2)
        return $ compose ts ts'
    (TInt, TFunc _ _) -> throwError (TypeMismatch "Int vs Function")
    (TFunc _ _, TInt) -> throwError (TypeMismatch "Int vs Function")

bindVar :: MonadError TypeError m => TypeVar -> T' -> m TSub
bindVar i (TVar j)
    | i == j = return M.empty
bindVar i t = if i `occursIn` t then throwError OccurCheck else return (M.singleton i t)

instantiate :: MonadState Counter m => S' -> m T'
instantiate (Forall tvs tp) = do
    ftvs <- mapM (const fresh) (S.toList tvs)
    return (apply (M.fromList $ zip (S.toList tvs) ftvs) tp)

occursIn :: TypeVar -> T' -> Bool
occursIn i = \case
    TInt -> False
    TVar j -> i == j
    TFunc at rt -> i `occursIn` at || i `occursIn` rt

generalize :: TEnv -> T' -> S'
generalize env t = Forall (ftv t S.\\ ftvTEnv env) t

ftv :: T' -> Set TypeVar
ftv = \case
    TInt -> S.empty
    TVar i -> S.singleton i
    TFunc arg res -> S.union (ftv arg) (ftv res)

ftvS' :: S' -> Set TypeVar
ftvS' (Forall tvs tp) = ftv tp `S.difference` tvs

ftvTEnv :: TEnv -> Set TypeVar
ftvTEnv = S.unions . map ftvS' . M.elems