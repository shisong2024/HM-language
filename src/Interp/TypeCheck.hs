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
import Data.Functor ((<&>))
import Control.Monad.Reader (MonadReader (ask))
import Data.Text (Text)
import Text.Megaparsec (unPos, SourcePos (sourceLine, sourceColumn))

typeChecker :: (MonadState Counter m, MonadReader DEnv m, MonadError (Located TypeError) m) => TEnv -> E' -> m (TSub, T')
typeChecker env = \case
    ILit _ -> return (M.empty, TInt)

    BLit _ -> return (M.empty, TBool)

    Var t -> case env !? t of
        Nothing -> throwError (Located Nothing $ UnboundVar t)
        Just s  -> do
            tp <- instantiate s
            return (M.empty, tp)

    ListLit es -> case es of
        [] -> do
            v <- fresh
            return (M.empty, TList v)
        e0: rest -> do
            (ts0, t0) <- typeChecker env e0
            let step ts e = do
                    (ts1, tp1) <- typeChecker (applyTEnv ts env) e
                    ts2 <- unify (apply ts1 (apply ts t0), apply ts1 tp1)
                    return (compose ts2 (compose ts1 ts))
            ts <- foldM step ts0 rest
            return (ts, TList (apply ts t0))
    
    TupleLit es -> do
        let step (ts, tps) e = do
                (ts1, tp1) <- typeChecker (applyTEnv ts env) e
                return (compose ts1 ts, tps ++ [tp1])
        (ts, tps) <- foldM step (M.empty, []) es
        return (ts, TTuple (fmap (apply ts) tps))
    
    BOpr bopr e1 e2 -> do
        (ts1, tp1) <- typeChecker env e1
        (ts2, tp2) <- typeChecker (applyTEnv ts1 env) e2
        case bopr of
            OCmp op | op == OpEq || op == OpNe -> do
                ts3 <- unify (apply ts2 tp1, apply ts2 tp2)
                return (compose ts3 (compose ts2 ts1), TBool)
            _ -> do
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
        let recEnv = case stripAt e1 of
                Lambda _ _ -> M.insert t (Forall S.empty tv) env
                _ -> env
        (ts1, tp1) <- typeChecker recEnv e1
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
        let tsf = compose ts5 $ compose ts4 ts'
        return (tsf, apply tsf tp1)

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

    Match e arms -> do
        (ts1, t) <- typeChecker env e
        (ts, mtp) <- foldM (matchType env t) (ts1, Nothing) arms
        denv <- ask
        checkNonExhaustive denv (apply ts t) arms
        case mtp of
            Just tp -> return (ts, tp)
            Nothing -> fresh <&> (ts, )

    AnnT e t -> do
        denv <- ask
        checkTypeCons denv t
        (ts, tp) <- typeChecker env e
        let vs = S.toList (ftv t)
        fs <- mapM (const fresh) vs
        let t' = apply (M.fromList (zip vs fs)) t
        ts' <- unify (apply ts tp, t')
        return (compose ts' ts, apply ts' t')

    At sp e -> catchError (typeChecker env e) (throwError . addSpan sp)

sccChecker :: (MonadState Counter m, MonadReader DEnv m, MonadError (Located TypeError) m) => TEnv -> [Binding] -> m (TSub, TEnv)
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

programChecker :: (MonadState Counter m, MonadReader DEnv m, MonadError (Located TypeError) m) => TEnv -> Program -> m (TSub, TEnv, M.Map Int StmtTy)
programChecker outer prs = do
    let datas = dataDeclsOf prs
        ctorEnv = M.unions (fmap ctorSchemes datas)
    checkDataDecls datas
    let outer' = M.union ctorEnv outer
    case fstDuplicate [n | StmtDef (Def n _ _) <- prs] of
        Just tx -> throwError $ Located Nothing $ DuplicateDef tx
        Nothing -> do
            (ts, nTEnv) <- go M.empty outer' (stronglyConnComp $ deps prs)
            let env' = applyTEnv ts (M.union nTEnv outer')
            exprs <- forM [(i, e) | (i, StmtExpr e) <- zip [0..] prs] $ \(i, e) -> (i, ) <$> tryEnv (fmap (apply ts . snd) (typeChecker env' e))
            let defTps = M.fromList [(i, TyDef n (nTEnv M.! n)) | (i, StmtDef (Def n _ _)) <- zip [0..] prs]
            return (ts, M.union ctorEnv nTEnv, M.union defTps (M.fromList (map (fmap TyExpr) exprs)))
    where
        go :: (MonadState Counter m, MonadReader DEnv m, MonadError (Located TypeError) m) => TSub -> TEnv -> [SCC Decl] -> m (TSub, TEnv)
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
    TInt -> TInt
    TBool -> TBool
    TVar i -> case ts !? i of
        Nothing -> TVar i
        Just (TVar j) | j == i -> TVar i
        Just tp -> apply ts tp
    TFunc argt rest -> TFunc (apply ts argt) (apply ts rest)
    TList tp -> TList (apply ts tp)
    TTuple tps -> TTuple (fmap (apply ts) tps)
    TCon n arg -> TCon n (fmap (apply ts) arg)

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
    (TList t1, TList t2) -> unify (t1, t2)
    (TCon n1 a1, TCon n2 a2) | n1 == n2 && length a1 == length a2 ->
        let step ts (x, y) = unify (apply ts x, apply ts y) >>= \ts1 -> return $ compose ts1 ts in
        foldM step M.empty (zip a1 a2)
    (TTuple t1, TTuple t2) -> do
        if length t1 /= length t2 then throwError $ Located Nothing $ TypeMismatch (TTuple t1) (TTuple t2)
        else let step ts (x, y) = unify (apply ts x, apply ts y) >>= \ts1 -> return (compose ts1 ts) in
            foldM step M.empty (zip t1 t2)
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
    TInt -> False
    TBool -> False 
    TVar j -> i == j
    TList tp -> i `occursIn` tp
    TTuple tps -> any (occursIn i) tps
    TFunc at rt -> i `occursIn` at || i `occursIn` rt
    TCon _ args -> any (occursIn i) args

generalize :: TEnv -> T' -> S'
generalize env t = Forall (ftv t S.\\ ftvTEnv env) t

ftv :: T' -> Set TypeVar
ftv = \case
    TInt -> S.empty
    TBool -> S.empty
    TVar i -> S.singleton i
    TList tp -> ftv tp
    TTuple tps -> S.unions (fmap ftv tps)
    TFunc arg res -> S.union (ftv arg) (ftv res)
    TCon _ args -> S.unions (fmap ftv args)

ftvS' :: S' -> Set TypeVar
ftvS' (Forall tvs tp) = ftv tp `S.difference` tvs

ftvTEnv :: TEnv -> Set TypeVar
ftvTEnv = S.unions . map ftvS' . M.elems

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
    TVar _ -> return ()
    TList t -> checkTypeCons denv t
    TTuple ts -> mapM_ (checkTypeCons denv) ts
    TFunc a b -> checkTypeCons denv a >> checkTypeCons denv b
    TCon n as -> do
        case M.lookup n (denvDatas denv) of
            Nothing -> throwError $ Located Nothing $ UnknownTypeCtor n
            Just _ -> case typeCtorArity denv n of
                Just k | k /= length as -> throwError $ Located Nothing $ TypeCtorArityMisMatch n k (length as)
                _ -> return ()
        mapM_ (checkTypeCons denv) as

errLine :: Located TypeError -> Int
errLine = \case
    Located (Just sp) _ -> fst (spanStart sp)
    _ -> 1

spanStart :: Span -> (Int, Int)
spanStart (Span s _) = (unPos (sourceLine s), unPos (sourceColumn s))

fstDuplicate :: Eq a => [a] -> Maybe a
fstDuplicate = \case
    [] -> Nothing
    x: xs -> if x `elem` xs then Just x else fstDuplicate xs

patType :: (MonadState Counter m, MonadReader DEnv m, MonadError (Located TypeError) m) => P' -> m (TSub, T', TEnv)
patType = \case
    PWild -> fresh <&> (M.empty, , M.empty)
    PVar x -> fresh >>= \v -> return (M.empty, v, M.singleton x (Forall S.empty v))
    PInt _ -> return (M.empty, TInt, M.empty)
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
        step :: (MonadState Counter m, MonadReader DEnv m, MonadError (Located TypeError) m) => (TSub, [T'], TEnv) -> P' -> m (TSub, [T'], TEnv)
        step (ts, tps, tenv) p = do
            (ts1, tp1, tenv1) <- patType p
            let ts' = compose ts1 ts
            return (ts', tps ++ [apply ts' tp1], M.union (applyTEnv ts' tenv1) tenv)
        
        step' :: (MonadState Counter m, MonadReader DEnv m, MonadError (Located TypeError) m) => (TSub, TEnv) -> (P', T') -> m (TSub, TEnv)
        step' (ts, tenv) (p, argT) = do
            (ts1, tp1, tenv1) <- patType p
            let ts' = compose ts1 ts
            ts2 <- unify (apply ts' tp1, apply ts' argT)
            let ts'' = compose ts2 ts'
            return (ts'', M.union (applyTEnv ts'' tenv1) (applyTEnv ts'' tenv))

matchType :: (MonadState Counter m, MonadReader DEnv m, MonadError (Located TypeError) m) => TEnv -> T' -> (TSub, Maybe T') -> (P', E') -> m (TSub, Maybe T')
matchType env t (ts, mtp) (p, b) = do
    checkDupPatVar p
    let env' = applyTEnv ts env
    (ts2, tp, benv) <- patType p
    let tsA = compose ts2 ts
    ts3 <- unify (apply tsA tp, apply tsA t)
    let tsB = compose ts3 tsA
    (ts4, tb) <- typeChecker (M.union (applyTEnv tsB benv) env') b
    let tsC = compose ts4 tsB
    case mtp of
        Nothing -> return (tsC, Just (apply tsC tb))
        Just t0 -> do 
            ts5 <- unify (apply tsC t0, apply tsC tb)
            let tsD = compose ts5 tsC
            return (tsD, Just (apply tsD tb))

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

checkNonExhaustive :: MonadError (Located TypeError) m => DEnv -> T' -> [(P', E')] -> m ()
checkNonExhaustive denv t arms
    | exhaustive denv t (map fst arms) = return ()
    | otherwise = throwError $ Located Nothing $ NonExhaustivePat t

exhaustive :: DEnv -> T' -> [P'] -> Bool
exhaustive denv t ps = any irrefutable ps || case t of
    TInt -> True
    TVar _ -> True
    TFunc _ _ -> True
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
                               

buildDEnv :: [DataDecl] -> DEnv
buildDEnv ds = DEnv
    { denvCtors = M.fromList [(c, ctorInfo d args) | d <- ds, (c, args) <- dCtors d]
    , denvDatas = M.fromList [(dName d, map fst (dCtors d)) | d <- ds]
    }

ctorInfo :: DataDecl -> [T'] -> CtorInfo
ctorInfo d args = let tvs = S.fromList [0..length (dParams d) - 1] in CtorInfo
    { ciTvs  = tvs
    , ciRes  = TCon (dName d) [TVar i | i <- S.toList tvs]
    , ciArgs = args
    }

ctorSchemes :: DataDecl -> TEnv
ctorSchemes d = M.fromList 
    [ (c, Forall (ciTvs ci) (foldr TFunc (ciRes ci) (ciArgs ci)))
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

