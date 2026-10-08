{-# LANGUAGE LambdaCase, FlexibleContexts, TupleSections, OverloadedStrings, ConstraintKinds #-}
module Interp.TypeCheck where

import Interp.Eval
import Interp.Types

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad (foldM, forM, forM_)
import Control.Monad.Error.Class (MonadError (throwError, catchError))
import Control.Monad.State (MonadState, get, put)
import Control.Monad.Reader (MonadReader (ask))

import Data.List (nub)
import Data.Map ((!?))
import Data.Set (Set)
import Data.Graph (stronglyConnComp)
import Data.Functor ((<&>))
import Data.Text (Text, pack)

import Text.Megaparsec (unPos, SourcePos (sourceLine, sourceColumn))

type Checker m = (MonadState Counter m, MonadReader DEnv m, MonadError (Located TypeError) m)

typeChecker :: Checker m => TEnv -> E' -> m (TSub, T')
typeChecker env e = do
    r <- inferExpr env [] e
    case irWanted r of
        [] -> return (irSub r, irType r)
        w: _ -> throwError $ Located (wtdSpan w)
            (AmbiguousImpType (apply (irSub r) (wtdType w)))

typeOfExpr :: Checker m => TEnv -> IEnv -> E' -> m S'
typeOfExpr env ienv e = do
    r <- inferExpr env ienv e
    fst <$> closeBinding env (irType r) (irWanted r) (irExpr r)

inferExpr :: Checker m => TEnv -> IEnv -> E' -> m InferRes
inferExpr env ienv e = case e of
    ILit _ -> return $ pureInfer e TInt

    BLit _ -> return $ pureInfer e TBool

    SLit _ -> return $ pureInfer e TString

    Var t -> case env !? t of
        Nothing -> throwError (Located Nothing $ UnboundVar t)
        Just s  -> do
            (preds, ty) <- instantiate s
            holes <- mapM (const freshHole) preds
            let ws = [ Wanted 
                    { wtdHole = h
                    , wtdType = predType p
                    , wtdScope = ienv
                    , wtdSpan = Nothing
                    , wtdName = predName p
                    , wtdBind = Nothing
                    } | (h, p) <- zip holes preds]
                e'  = foldl App e (map ImpHole holes)
            return $ InferRes M.empty ws ty e'

    ListLit es -> case es of
        [] -> pureInfer e . TList <$> fresh
        e0: rest -> do
            r0 <- inferExpr env ienv e0
            let step (su, ws, elemTy, es') ex = do
                    r <- inferExpr (applyTEnv su env) ienv ex
                    u <- unify (apply (irSub r) (apply su elemTy), apply (irSub r) (irType r))
                    let su' = compose u (compose (irSub r) su)
                    return (su', ws <> irWanted r, elemTy, irExpr r: es')
            (sub, wanted, ty, revE) <- foldM step (irSub r0, irWanted r0, irType r0, [irExpr r0]) rest
            return $ InferRes
                { irSub    = sub
                , irWanted = map (applyWanted sub) wanted
                , irType   = TList (apply sub ty)
                , irExpr   = ListLit (reverse revE)
                }
    
    TupleLit es -> do
        let step (su, ws, tys, done) ex = do
                r <- inferExpr (applyTEnv su env) ienv ex
                let su' = compose (irSub r) su
                return (su', ws <> irWanted r, tys <> [irType r], done <> [irExpr r])
        (su, ws, tys, es') <- foldM step (M.empty, [], [], []) es
        return $ InferRes
            { irSub    = su
            , irWanted = map (applyWanted su) ws
            , irType   = TTuple (map (apply su) tys)
            , irExpr   = TupleLit es'
            }
    
    BOpr op a b -> do
        ra <- inferExpr env ienv a
        rb <- inferExpr env ienv b
        let su0 = compose (irSub rb) (irSub ra)
        case op of
            OCmp cmp | cmp == OpEq || cmp == OpNe -> do
                u <- unify (apply su0 (irType ra), apply su0 (irType rb))
                let su = compose u su0
                return $ InferRes su (map (applyWanted su) (irWanted ra <> irWanted rb))
                    TBool (BOpr op (irExpr ra) (irExpr rb))
            _ -> do
                u1 <- unify (apply su0 (irType ra), TInt)
                u2 <- unify (apply u1 (apply su0 (irType rb)), TInt)
                let su = compose u2 (compose u1 su0)
                    out = case op of
                        OArith _ -> TInt
                        OCmp _   -> TBool
                return $ InferRes su (map (applyWanted su) (irWanted ra <> irWanted rb))
                    out (BOpr op (irExpr ra) (irExpr rb))

    Let n rhs body -> inferLet env ienv n rhs body
    
    DictCall h as ds -> inferDictCall env ienv h as ds         

    If c yes no -> do
        rc <- inferExpr env ienv c
        u0 <- unify (irType rc, TBool)
        let su0 = compose u0 (irSub rc)
        ry <- inferExpr (applyTEnv su0 env) ienv yes
        let su1 = compose (irSub ry) su0
        rn <- inferExpr (applyTEnv su1 env) ienv no
        let su2 = compose (irSub rn) su1
        u3 <- unify (apply su2 (irType ry), apply su2 (irType rn))
        let su = compose u3 su2
        return $ InferRes su (map (applyWanted su) (irWanted rc <> irWanted ry <> irWanted rn))
            (apply su (irType ry)) (If (irExpr rc) (irExpr ry) (irExpr rn))

    Lambda n body -> do
        tv <- fresh
        let env' = M.insert n (Forall S.empty [] tv) env
        r <- inferExpr env' ienv body
        return $ r { irType = TFunc (apply (irSub r) tv) (irType r), irExpr = Lambda n (irExpr r) }

    App f arg -> do
        rf <- inferExpr env ienv f
        ra <- inferExpr (applyTEnv (irSub rf) env) ienv arg
        out <- fresh
        let su0 = compose (irSub ra) (irSub rf)
        u <- unify (apply su0 (irType rf), TFunc (apply su0 (irType ra)) out) 
            `catchError` calleeNote (applyTEnv su0 env) f
        let su = compose u su0
        return $ InferRes su (map (applyWanted su) (irWanted rf <> irWanted ra))
            (apply su out) (App (irExpr rf) (irExpr ra))


    Match scrut arms -> do
        rs <- inferExpr env ienv scrut
        (su, ws, out, arms') <- foldM (inferArm env ienv (irType rs)) (irSub rs, irWanted rs, Nothing, []) arms
        denv <- ask
        checkNonExhaustive denv (apply su (irType rs)) arms
        outTy <- maybe fresh (return . apply su) out
        return $ InferRes su (map (applyWanted su) ws) outTy (Match (irExpr rs) arms')

    AnnT ex ann -> do
        denv <- ask
        checkTypeCons denv ann
        r <- inferExpr env ienv ex
        let vs = S.toList (ftv ann)
        fs <- mapM (const fresh) vs
        let ann' = apply (M.fromList (zip vs fs)) ann
        u <- unify (apply (irSub r) (irType r), ann')
        return (applyInfer u r) { irExpr = AnnT (irExpr r) ann }

    At sp ex -> do
        r <- inferExpr env ienv ex `catchError` (throwError . addSpan sp)
        return r { irWanted = map (underSpan sp) (irWanted r), irExpr = At sp (irExpr r) }

    ImpHole h -> throwError (Located Nothing (EscapedImpHole h))

sccChecker :: Checker m => TEnv -> [Binding] -> m (TSub, TEnv)
sccChecker env bs = do
    tvs <- mapM (const fresh) bs
    let ns   = map fst bs
        genv = M.union (M.fromList (zip ns (map (Forall S.empty []) tvs))) env
        bds  = map snd bs
    res <- mapM (\(n, b) -> withNote ("in definition of `" <> n <> "`" ) (typeChecker genv b)) (zip ns bds)
    let tps = map snd res
        ts0 = foldr (compose .fst) M.empty res
    ts <- foldM 
        (\acc (tv, tp) -> do
            d <- unify (apply acc tv, apply acc tp)
            return $ compose d acc
        ) ts0 (zip tvs tps)
    let env' = applyTEnv ts env
        sms  = [(n, generalize env' [] (apply ts tp)) | (n, tp) <- zip ns tps]
    return (ts, M.fromList sms)

programChecker :: Checker m => TEnv -> IFrame -> Program -> m CProgram
programChecker outerT outerI program = do
    let datas = dataDeclsOf program
        ctorEnv = M.unions (map ctorSchemes datas)
        outer = M.union ctorEnv outerT
        implicitDecls = [(True, d) | StmtImp d <- program]
        ordinaryDecls = [(False, d) | StmtDef d <- program]
        impNames = [n | (True, Def n _ _ ) <- implicitDecls]
        ordNames = [n | (False, Def n _ _) <- ordinaryDecls]
        allDecls = implicitDecls <> ordinaryDecls
        names = [n | (_, Def n _ _) <- allDecls]

    checkDataDecls datas
    case fstDuplicate names of
        Just n  -> throwError (Located Nothing (DuplicateDef n))
        Nothing -> return ()

    provTs <- mapM (const fresh) ordNames
    let provT = M.fromList [(n, Forall S.empty [] t) | (n, t) <- zip ordNames provTs]
    (_, bs, res, fd) <- sccCheckerElab outer [outerI] ordinaryDecls

    (impBingdings, ordBindings, impT, defRes, failed) <- 
        if null implicitDecls then return ([], bs, M.empty, res, fd)
        else do
            let ordT = M.union (M.fromList [(cName b, cScheme b) | b <- bs]) provT
            (_, bs2, res2, _) <- sccCheckerElab (M.union ordT outer) (impNames: [outerI]) implicitDecls
            forM_ bs2 $ \b -> case cScheme b of
                sch@(Forall vs ps ty)
                    | not (S.null vs) || not (null ps) || not (isClosedType ty) -> 
                        throwError $ Located Nothing $ ImpCandidateHasContext (cName b) sch
                    | otherwise -> return ()
            let impT = M.fromList [(cName b, cScheme b) | b <- bs2]
            (_, bs3, res3, fd3) <- sccCheckerElab (M.union impT outer) (impNames: [outerI]) ordinaryDecls
            return (bs2, bs3, impT, M.union res2 res3, fd3)

    let allBindings = impBingdings <> ordBindings
        newT = M.union impT (M.fromList [(cName b, cScheme b) | b <- ordBindings])
        env = M.union newT outer
        scope = impNames: [outerI]
    exprRes <- forM [(i, e) | (i, StmtExpr e) <- zip [0..] program] $ \(i, e) -> do
        let blocked = [n | n <- S.toList (freeVars e), S.member n failed]
        case blocked of
            n: _ -> return (i, Left $ Located Nothing $ CalleeNotChecked n)
            [] -> do
                r <- tryEnv $ do
                    rr <- inferExpr env scope e
                    let ws = map (applyWanted (irSub rr)) (irWanted rr)
                    solved <- forM ws $ \w -> resolveWanted env w <&> ((wtdHole w,) . Var)
                    e' <- case fillHoles (M.fromList solved) (irExpr rr) of
                        Left h -> throwError $ Located Nothing $ EscapedImpHole h
                        Right x -> return x
                    return (apply (irSub rr) (irType rr), e')
                return (i, r)

    let bodyByName = M.fromList
            [(cName b, cBody b) | b <- allBindings]
        rebuild n d = case M.lookup n bodyByName of
            Nothing -> d
            Just b -> Def n [] b
        elaborateStmt stmt = case stmt of
            StmtDef d@(Def n _ _) -> StmtDef (rebuild n d)
            StmtImp d@(Def n _ _) -> StmtDef (rebuild n d)
            other -> other
        exprByIndex = M.fromList [(i, e) | (i, Right (_, e)) <- exprRes]
        elaborated =
            [ case stmt of
                StmtExpr e -> StmtExpr (M.findWithDefault e i exprByIndex)
                _          -> elaborateStmt stmt
            | (i, stmt) <- zip [0..] program
            ]
        typeMap = M.fromList
            [ (i, M.findWithDefault (TyDefSkipped []) n defRes)
            | (i, stmt) <- zip [0..] program
            , n <- case stmt of
                StmtDef d -> [declName d]
                StmtImp d -> [declName d]
                _         -> []
            ] <> M.fromList
            [(i, TyExpr (fmap fst r)) | (i, r) <- exprRes]

    return CProgram
        { cSub     = M.empty
        , cTEnv    = M.union ctorEnv newT
        , cTypes   = typeMap
        , cProgram = elaborated
        , cIFrame  = impNames
        }

declName :: Decl -> Text
declName (Def n _ _) = n

sccCheckerElab :: Checker m => TEnv -> IEnv -> [(Bool, Decl)] -> m (TSub, [CBinding], M.Map Text StmtTy, Set Text)
sccCheckerElab outer ienv declarations = do
    let declMap = M.fromList [(n, flag) | (flag, Def n _ _) <- declarations]
        programForDeps = [StmtDef d | (_, d) <- declarations]
        ds0 = deps programForDeps
        depsOf = M.fromList [(n, xs) | (_, n, xs) <- ds0]
        groups = stronglyConnComp ds0
    (ts, bs, res, fd) <- foldM (checkGroup declMap depsOf) (M.empty, [], M.empty, S.empty) groups
    return (ts, bs, res, fd)
  where
    checkGroup declMap depsOf (ts, done, res, failed) scc = do
        let ds = flatten scc
            names = [n | Def n _ _ <- ds]
            brk = [d | n <- names, d <- M.findWithDefault [] n depsOf, d `notElem` names, S.member d failed]
            skipWith xs = (ts, done, M.union (M.fromList xs) res, S.union (S.fromList names) failed)
        if not (null brk) then return (skipWith [(n, TyDefSkipped brk) | n <- names])
        else do
            r <- tryEnv (checkOne declMap done ds)
            case r of
                Right (ts1, bs) -> 
                    return (compose ts1 ts, done <> bs, 
                        foldr (\b m -> M.insert (cName b) (TyDef (cName b) (cScheme b)) m) res bs, failed)
                Left err -> return $ skipWith $ case names of
                    [] -> []
                    (n0: more) -> (n0, TyDefFailed err): [(n, TyDefSkipped (filter (/= n) more)) | n <- more]
        
    checkOne declMap done ds = do
        let envDone = M.union (M.fromList [(cName b, cScheme b) | b <- done]) outer
        (sub, bindings) <- inferBindingGroup envDone ienv ds
        return (sub, [CBinding n sch b (M.findWithDefault False n declMap) | (n, sch, b) <- bindings])

fresh :: MonadState Counter m => m T'
fresh = get >>= \n -> put (n + 1) >> return (TVar n)

apply :: TSub -> T' -> T'
apply ts = \case
    TInt -> TInt
    TBool -> TBool
    TString -> TString
    TVar i -> case ts !? i of
        Nothing -> TVar i
        Just (TVar j) | j == i -> TVar i
        Just tp -> apply ts tp
    TFunc argt rest -> TFunc (apply ts argt) (apply ts rest)
    TList tp -> TList (apply ts tp)
    TTuple tps -> TTuple (fmap (apply ts) tps)
    TCon n arg -> TCon n (fmap (apply ts) arg)

applyPred :: TSub -> Pred -> Pred
applyPred ts = \case
    Implicit t -> Implicit (apply ts t)
    NameImplicit n t -> NameImplicit n (apply ts t)

applyS' :: TSub -> S' -> S'
applyS' ts (Forall vs ps t) =
    let ts' = foldr M.delete ts vs
    in Forall vs (map (applyPred ts') ps) (apply ts' t)

applyTEnv :: TSub -> TEnv -> TEnv
applyTEnv = M.map . applyS'

applyWanted :: TSub -> Wanted -> Wanted
applyWanted ts w = w { wtdType = apply ts (wtdType w) }

applyInfer :: TSub -> InferRes -> InferRes
applyInfer ts r = r 
    { irSub    = compose ts (irSub r)
    , irWanted = map (applyWanted ts) (irWanted r)
    , irType   = apply ts (irType r)
    }

pureInfer :: E' -> T' -> InferRes
pureInfer e t = InferRes M.empty [] t e

underSpan :: Span -> Wanted -> Wanted
underSpan sp w = case wtdSpan w of
    Nothing -> w { wtdSpan = Just sp }
    Just _  -> w

mapInferExpr :: (E' -> E') -> InferRes -> InferRes
mapInferExpr f r = r { irExpr = f (irExpr r) }

freshHole :: MonadState Counter m => m HoleId
freshHole = get >>= \n -> put (n + 1) >> return n

compose :: TSub -> TSub -> TSub
compose s2 s1 = M.union (M.map (apply s2) s1) s2

unify :: MonadError (Located TypeError) m => (T', T') -> m TSub
unify = \case
    (TInt, TInt) -> return M.empty
    (TBool, TBool) -> return M.empty
    (TString, TString) -> return M.empty
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

instantiate :: MonadState Counter m => S' -> m ([Pred], T')
instantiate (Forall vs ps tp) = do
    ftvs <- mapM (const fresh) (S.toList vs)
    let su = M.fromList $ zip (S.toList vs) ftvs
    return (map (applyPred su) ps, apply su tp)
   
occursIn :: TypeVar -> T' -> Bool
occursIn i = \case
    TInt -> False
    TBool -> False 
    TString -> False
    TVar j -> i == j
    TList tp -> i `occursIn` tp
    TTuple tps -> any (occursIn i) tps
    TFunc at rt -> i `occursIn` at || i `occursIn` rt
    TCon _ args -> any (occursIn i) args

generalize :: TEnv -> [Pred] -> T' -> S'
generalize env ps t = 
    let vars = (ftv t `S.union` ftvPreds ps) `S.difference` ftvTEnv env
    in Forall vars (dedupPreds ps) t

dedupPreds :: [Pred] -> [Pred]
dedupPreds = S.toList . S.fromList

ftv :: T' -> Set TypeVar
ftv = \case
    TInt -> S.empty
    TBool -> S.empty
    TString -> S.empty
    TVar i -> S.singleton i
    TList tp -> ftv tp
    TTuple tps -> S.unions (fmap ftv tps)
    TFunc arg res -> S.union (ftv arg) (ftv res)
    TCon _ args -> S.unions (fmap ftv args)

ftvPred :: Pred -> Set TypeVar
ftvPred = ftv . predType

ftvPreds :: [Pred] -> Set TypeVar
ftvPreds = S.unions . map ftvPred

ftvS' :: S' -> Set TypeVar
ftvS' (Forall vs ps t) =(ftv t `S.union` ftvPreds ps) `S.difference` vs

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
    TString -> return ()
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

checkNonExhaustive :: MonadError (Located TypeError) m => DEnv -> T' -> [(P', E')] -> m ()
checkNonExhaustive denv t arms
    | exhaustive denv t (map fst arms) = return ()
    | otherwise = throwError $ Located Nothing $ NonExhaustivePat t

exhaustive :: DEnv -> T' -> [P'] -> Bool
exhaustive denv t ps = any irrefutable ps || case t of
    TInt -> True
    TString -> True
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
    [ (c, Forall (ciTvs ci) [] (foldr TFunc (ciRes ci) (ciArgs ci)))
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

withNote :: MonadError (Located TypeError) m => Text -> m a -> m a
withNote nt act = catchError act $ \(Located sp err) -> throwError (Located sp $ WithNote nt err)

spinHead :: E' -> Maybe Text
spinHead = \case
    Var n -> Just n
    At _ e -> spinHead e
    App e _ -> spinHead e
    AnnT e _ -> spinHead e
    _ -> Nothing

calleeNote :: MonadError (Located TypeError) m => M.Map Text S' -> E' -> Located TypeError -> m a
calleeNote env f e@(Located sp err) = case spinHead f of
    Just n | Just sch <- env !? n, informative sch -> throwError $ Located sp $ CalleeNote n sch err
    _ -> throwError e
    where
        bareVar :: T' -> Bool
        bareVar = \case
            TVar _ -> True
            _ -> False

        informative :: S' -> Bool
        informative (Forall tvs _ tp) = not (null tvs) || not (bareVar tp)

swallowedArmsNote :: Text
swallowedArmsNote = 
       "This arm's pattern does not fit the scrutinee's type. One common cause:\n" 
    <> "if the arm above has an unparenthesised `match` in its body, that inner "
    <> "`match` has absorbed this `|` arm\n"
    <> "-- put parentheses around it to keep the arms where you meant them."

inferLet :: Checker m => TEnv -> IEnv -> Text -> E' -> E' -> m InferRes
inferLet env ienv n rhs body = do
    (su, sch, rhsIR) <- case stripAt rhs of
        Lambda {} -> do
            (sub, bds) <- inferBindingGroupWithNotes False env ienv [Def n [] rhs]
            case bds of
                [(_, ty, e)] -> return (sub, ty, e)
                _ -> throwError $ Located Nothing DictConstrainDidNotConverge
        _ -> do
            rr <- inferExpr env ienv rhs
            let env1 = applyTEnv (irSub rr) env
            (ty, e) <- closeBinding env1 (irType rr) (irWanted rr) (irExpr rr)
            return (irSub rr, ty, e)
    rb <- inferExpr (M.insert n sch (applyTEnv su env)) ienv body
    return $ rb
        { irSub    = compose (irSub rb) su
        , irExpr = Let n rhsIR (irExpr rb)
        }

inferArm :: Checker m => TEnv -> IEnv -> T' -> (TSub, [Wanted], Maybe T', [(P', E')]) -> (P', E') -> m (TSub, [Wanted], Maybe T', [(P', E')])
inferArm env ienv scrutTy (su, ws, oldout, done) (pat, body) = do
    checkDupPatVar pat
    (ps, patT, patE) <- patType pat
    let su0  = compose ps su
        note = case oldout of
            Just _  -> withNote swallowedArmsNote
            Nothing -> id
    u0 <- note $ unify (apply su0 patT, apply su0 scrutTy)
    let su1 = compose u0 su0
        bdE = M.union (applyTEnv su1 patE) (applyTEnv su1 env)
    rb <- inferExpr bdE ienv body
    let su2 = compose (irSub rb) su1
    (su3, out) <- case oldout of
        Nothing -> return (su2, irType rb)
        Just t  -> do
            u <- unify (apply su2 t, apply su2 (irType rb))
            return (compose u su2, apply u (irType rb))
    return (su3, ws <> irWanted rb, Just out, done <> [(pat, irExpr rb)])

inferDictCall :: Checker m => TEnv -> [IFrame] -> E' -> [E'] -> [DictArg] -> m InferRes
inferDictCall env ienv head0 ordinary dicts = do
    let (headE, prefix) = callSpine head0
    name <- maybe (throwError $ Located Nothing DictInterfaceUnavailable) return (dictHeadName headE)
    let hints = M.lookup ("$dictHint@" <> name) env
        hEnv  = case (M.lookup name env, hints) of
            (Just (Forall vs ps t), Just (Forall _ hs _)) -> 
                let extra = [p | p <- hs, predName p `notElem` map predName ps]
                in M.insert name (Forall vs (ps <> extra) t) env
            _ -> env
    case (M.lookup name hEnv, hints) of
        (Just (Forall _ [] (TVar _)), Nothing) -> throwError $ Located Nothing DictInterfaceUnavailable
        _ -> return ()
    case fstDuplicate (map daName dicts) of
        Just n  -> throwError $ Located (dictSpan n) (DuplicateDictArg n)
        Nothing -> return ()
    rf <- inferExpr hEnv ienv headE
    let slots = [DictSlot (wtdName w) (wtdType w) (wtdHole w) | w <- irWanted rf]
        avail = [n | DictSlot (Just n) _ _ <- slots]
    chosen <- forM dicts $ \d -> case [s | s <- slots, dsName s == Just (daName d)] of
        [] -> throwError $ Located (Just (daSpan d)) $ UnknownDictArg (daName d) avail
        [s] -> return (d, s)
        _ -> throwError $ Located (Just (daSpan d)) $ AmbiguousDictArg (daName d)
    headT <- freshInternal "call"
    let headIR = foldl App (Var headT) [ImpHole (dsHole s) | s <- slots]
    appRes <- foldM inferArg (rf { irExpr = headIR }) (prefix <> ordinary)
    (su, rhsWs, sols, bds) <- foldM inferDict (irSub appRes, [], M.empty, []) chosen
    let exHoles = S.fromList (map (dsHole . snd) chosen)
        remaining = filter (not . (`S.member` exHoles) . wtdHole) (irWanted appRes)
        body = replaceSolvedHoles sols (irExpr appRes)
        lowered = Let headT headE (foldr (\(n, e) b -> Let n e b) body bds)
    return $ InferRes
        { irSub    = su
        , irWanted = map (applyWanted su) (remaining <> rhsWs)
        , irType   = apply su (irType appRes)
        , irExpr   = lowered
        }
    where
        dictSpan n = case [daSpan d | d <- dicts, daName d == n] of
            sp: _ -> Just sp
            _ -> Nothing

        inferArg acc arg = do
            ra <- inferExpr (applyTEnv (irSub acc) env) ienv arg
            out <- fresh
            let su0 = compose (irSub ra) (irSub acc)
            u <- unify (apply su0 (irType acc), TFunc (apply su0 (irType ra)) out)
                `catchError` calleeNote (applyTEnv su0 env) head0
            let su = compose u su0
            return $ InferRes
                { irSub    = su
                , irWanted = map (applyWanted su) (irWanted acc <> irWanted ra)
                , irType   = apply su out
                , irExpr   = App (irExpr acc) (irExpr ra)
                }
        
        inferDict (su, ws, solved, bs) (d, slot) = do
            rd <- inferExpr (applyTEnv su env) ienv (daExpr d)
            let su0 = compose (irSub rd) su
                actual = apply su0 (irType rd)
                expected = apply su0 (dsType slot)
            u <- unify (actual, expected) `catchError` \(Located _ reason) -> 
                throwError $ Located (Just (daSpan d)) (DictArgMismatch (daName d) actual expected reason)
            tmp <- freshInternal "dict"
            return (compose u su0, ws <> irWanted rd, 
                M.insert (dsHole slot) (Var tmp) solved, bs <> [(tmp, irExpr rd)])

inferBindingGroup :: Checker m => TEnv -> [IFrame] -> [Decl] -> m (TSub, [(Text, S', E')])
inferBindingGroup = inferBindingGroupWithNotes True

inferBindingGroupWithNotes :: Checker m => Bool -> TEnv -> [IFrame] -> [Decl] -> m (TSub, [(Text, S', E')])
inferBindingGroupWithNotes annoate outer ienv ds = do
    prepared <- forM ds $ \(Def n ps b) -> prepareParams ps >>= \ps' -> return (Def n ps' b)
    types <- mapM (const fresh) ds
    let names = map declName ds
        seeds = M.fromList 
            [(n, Forall S.empty [NameImplicit p t | ImplicitParam p t <- ps, S.member p (freeVars b)] ty) 
            | (Def n ps b, ty) <- zip prepared types]
    hints <- forM (zip names types) $ \(n, t) -> do
        let labels = nub [l | Def _ ps b <- prepared, (tar, l) <- dictCallHints (S.fromList (map paramName ps)) b, tar == n]
        ts <- mapM (const fresh) labels
        return ("$dictHint@" <> n, Forall S.empty (zipWith NameImplicit labels ts) t)
    settle 64 M.empty seeds prepared types (M.fromList hints)
    where
        note n action = if annoate then withNote ("in definition of " <> tick n) action else action

        settle fuel sub schs ppd types hs
            | fuel <= (0 :: Int) = throwError $ Located Nothing DictConstrainDidNotConverge
            | otherwise = do
                let renv = M.unions [applyTEnv sub schs, applyTEnv sub outer, applyTEnv sub hs]
                (su, rev) <- foldM (inferOne renv) (sub, []) (zip ppd types)
                let inferred = reverse rev
                    next = M.fromList [(n, Forall S.empty
                        (map fst (groupWanted (filter deferWanted (irWanted (applyInfer su r)))))
                        (apply su ty)) | ((n, r), ty) <- zip inferred types]
                if next /= applyTEnv su schs then settle (fuel - 1) su next ppd types hs
                else do
                    let strictEnv = M.union (applyTEnv su next) (applyTEnv su outer)
                    (finalSu, finalRev) <- foldM (inferOne strictEnv) (su, []) (zip ppd types)
                    checked <- forM (reverse finalRev) $ \(n, r0) -> do
                        let r = applyInfer finalSu r0
                        (sch, body) <- note n $ closeBinding (applyTEnv finalSu outer) (irType r) (irWanted r) (irExpr r)
                        return (n, sch, body)
                    return (finalSu, checked)
    
        inferOne renv (sub, done) (Def n ps b, ty) = do
            let ps' = map (\case ImplicitParam x t -> ImplicitParam x (apply sub t); p -> p) ps
            r <- note n $ inferDeclBodyPrepared (applyTEnv sub renv) ienv ps' b
            let su0 = compose (irSub r) sub
            u <- unify (apply su0 ty, apply su0 (irType r))
            return (compose u su0, (n, r): done)

prepareParams :: Checker m => [Param] -> m [Param]
prepareParams = mapM $ \case
    ImplicitParam n ann -> do
        denv <- ask
        checkTypeCons denv ann
        let vs = S.toList (ftv ann)
        fs <- mapM (const fresh) vs
        return (ImplicitParam n (apply (M.fromList (zip vs fs)) ann))
    ExplicitParam n -> return $ ExplicitParam n

freshInternal :: MonadState Counter m => Text -> m Text
freshInternal p = freshHole <&> (("$" <> p <> "@") <>) . pack . show

dictHeadName :: E' -> Maybe Text
dictHeadName = \case
    Var n -> Just n
    At _ e -> dictHeadName e
    AnnT e _ -> dictHeadName e
    _ -> Nothing

dictCallHints :: Set Text -> E' -> [(Text, Text)]
dictCallHints bd = go
    where
        go = \case
            DictCall h as ds -> 
                let (headE, _) = callSpine h
                    own = case stripAt headE of
                        Var n | not (S.member n bd) -> [(n, daName d) | d <- ds]
                        _ -> []
                in own <> concatMap go (h: as <> map daExpr ds)
            Let n rhs body -> 
                let recBd = case stripAt rhs of
                        Lambda _ _ -> S.insert n bd
                        _ -> bd
                in dictCallHints recBd rhs <> dictCallHints (S.insert n bd) body
            Lambda n body -> dictCallHints (S.insert n bd) body
            Match x arms -> go x <> concat 
                [dictCallHints (S.union bd (S.fromList (patVarsList pat))) body | (pat, body) <- arms]
            ListLit es -> concatMap go es
            TupleLit es -> concatMap go es
            If c a b -> concatMap go [c, a, b]
            BOpr _ a b -> go a <> go b
            App f a -> go f <> go a
            AnnT x _ -> go x
            At _ x -> go x
            _ -> []

callSpine :: E' -> (E', [E'])
callSpine e = case stripAt e of
    App f a -> let (h, as) = callSpine f in (h, as <> [a])
    _ -> (e, [])

replaceSolvedHoles :: HoleSol -> E' -> E'
replaceSolvedHoles solved e = let go = replaceSolvedHoles solved in case e of
    ImpHole h -> M.findWithDefault e h solved
    ILit _ -> e
    BLit _ -> e
    SLit _ -> e
    Var _ -> e
    ListLit es -> ListLit (map go es)
    TupleLit es -> TupleLit (map go es)
    Let n a b -> Let n (go a) (go b)
    DictCall h as ds -> DictCall (go h) (map go as) [d { daExpr = go (daExpr d) } | d <- ds]
    If c a b -> If (go c) (go a) (go b)
    BOpr op a b -> BOpr op (go a) (go b)
    Lambda n a -> Lambda n (go a)
    App f x -> App (go f) (go x)
    Match ex arms -> Match (go ex) [(p, go b) | (p, b) <- arms]
    AnnT ex t -> AnnT (go ex) t
    At sp ex -> At sp (go ex)

isClosedType :: T' -> Bool
isClosedType = S.null . ftv

deferWanted :: Wanted -> Bool
deferWanted w = case wtdName w of
    Just _ -> True
    Nothing -> not (isClosedType (wtdType w))

wantedPred :: Wanted -> Pred
wantedPred w = case wtdName w of
    Nothing -> Implicit $ wtdType w
    Just n  -> NameImplicit n (wtdType w)

resolveWanted :: Checker m => TEnv -> Wanted -> m Text
resolveWanted tenv wtd = case wtdName wtd of
    Nothing -> go (wtdScope wtd)
    Just n  -> byName n (wtdScope wtd)
    where
        target :: T'
        target = wtdType wtd
        
        go :: Checker m => [[Text]] -> m Text
        go = \case
            [] -> throwError $ Located (wtdSpan wtd) (NoImplicit target)
            frame: outer -> do
                let matches = [n | n <- frame, Just ty <- [candidate n], ty == target]
                case matches of
                    []     -> go outer
                    [name] -> return name
                    names  -> throwError $ Located (wtdSpan wtd)
                        (AmbiguousImp target names)

        candidate :: Text -> Maybe T'
        candidate n = case M.lookup n tenv of
            Just (Forall vars [] ty)
                | S.null vars && isClosedType ty -> Just ty
            _ -> Nothing

        byName :: Checker m => Text -> [[Text]] -> m Text
        byName n = \case
            [] -> go (wtdScope wtd)
            frame: outer
                | n `elem` frame -> case candidate n of
                    Just ty | ty == target -> return n
                    _ -> throwError $ Located (wtdSpan wtd) (NoImplicit target)

                | otherwise -> byName n outer

fillHoles :: HoleSol -> E' -> Either HoleId E'
fillHoles solved = go
    where
        go = \case
            ImpHole h -> maybe (Left h) Right (M.lookup h solved)
            ILit n -> Right (ILit n)
            BLit b -> Right (BLit b)
            SLit s -> Right (SLit s)
            Var n -> Right (Var n)
            ListLit es -> ListLit <$> mapM go es
            TupleLit es -> TupleLit <$> mapM go es
            Let n a b -> Let n <$> go a <*> go b
            DictCall h as ds -> DictCall <$> go h <*> mapM go as
                <*> mapM (\d -> go (daExpr d) >>= \e -> return d {daExpr = e}) ds
            If c a b -> If <$> go c <*> go a <*> go b
            BOpr op a b -> BOpr op <$> go a <*> go b
            Lambda n b -> Lambda n <$> go b
            App f x -> App <$> go f <*> go x
            Match e arms -> Match <$> go e
                <*> mapM (\(p, b) -> (,) p <$> go b) arms
            AnnT e t -> AnnT <$> go e <*> pure t
            At sp e -> At sp <$> go e

closeBinding :: Checker m => TEnv -> T' -> [Wanted] -> E' -> m (S', E')
closeBinding outer ty wanted0 body = do
    let genVars =
            (ftv ty `S.union` S.unions (map (ftv . wtdType) wanted0))
            `S.difference` ftvTEnv outer

        concrete = not . deferWanted
        propagates w = ftv (wtdType w) `S.isSubsetOf` genVars

        concreteWs = filter concrete wanted0
        outwardWs = filter (not . concrete) wanted0
        badWs = filter (not . propagates) outwardWs

    case badWs of
        w: _ -> throwError $ Located (wtdSpan w)
            (AmbiguousImpType (wtdType w))
        [] -> return ()

    concreteSolutions <- forM concreteWs $ \w -> do
        candidate <- resolveWanted outer w
        return (wtdBind w, wtdHole w, candidate)

    let grouped = groupWanted outwardWs
        hidden = zipWith mkHidden [0 :: Int ..] grouped
        mkHidden i (pred', ws) = 
            let bound = [n | w <- ws, Just n <- [wtdBind w]]
                primary = case bound of
                    n: _ -> n 
                    [] -> "$implicit" <> pack (show i)
            in (pred', primary, filter (/= primary) bound, ws)
        hiddenSolutions =
            [(wtdHole w, Var pri) | (_, pri, _, ws) <- hidden, w <- ws]
        solved = M.fromList ([(h, Var c) | (_, h, c) <- concreteSolutions] <> hiddenSolutions)
        predicates = [pred' | (pred', _, _, _) <- hidden]
        captured = [(n, c) | (Just n, _, c) <- concreteSolutions]

    body' <- case fillHoles solved body of
        Left h  -> throwError $ case [w | w <- concreteWs, wtdHole w == h] of
            w: _ -> Located (wtdSpan w) (EscapedImpHole h)
            [] -> Located Nothing (EscapedImpHole h)
        Right e -> return e

    let bodyB = foldr (\(n, c) b -> App (Lambda n b) (Var c)) body' captured
        aliasOne (_, pri, aliases, _) b = foldr (\x acc -> Let x (Var pri) acc) b aliases
        body'' = foldr (Lambda . (\(_, n, _, _) -> n)) (foldr aliasOne bodyB hidden) hidden
        scheme = Forall genVars predicates ty
    return (scheme, body'')

groupWanted :: [Wanted] -> [(Pred, [Wanted])]
groupWanted = foldl insert []
    where
        insert groups w =
            let p = wantedPred w
            in case break ((== p) . fst) groups of
                (before, []) -> before <> [(p, [w])]
                (before, (q, ws): after) -> before <> ((q, ws <> [w]): after)

inferDeclBodyPrepared :: Checker m => TEnv -> IEnv -> [Param] -> E' -> m InferRes
inferDeclBodyPrepared env ienv params body = go env params
    where
        go current = \case
            [] -> inferExpr current ienv body

            (ExplicitParam n : rest) -> do
                a <- fresh
                r <- go (M.insert n (Forall S.empty [] a) current) rest
                return r
                    { irType = TFunc (apply (irSub r) a) (irType r)
                    , irExpr = Lambda n (irExpr r)
                    }
            (ImplicitParam n annotation : rest) -> do
                denv <- ask
                checkTypeCons denv annotation
                r <- go (M.insert n (Forall S.empty [] annotation) current) rest
                if not (S.member n (freeVars (irExpr r))) then return r 
                else do
                    h <- freshHole
                    return r { irWanted = Wanted 
                        { wtdHole = h
                        , wtdType = annotation
                        , wtdScope = ienv 
                        , wtdSpan = bodySpan body
                        , wtdBind = Just n
                        , wtdName = Just n
                        } : irWanted r
                    }

bodySpan :: E' -> Maybe Span
bodySpan = \case 
    At sp _ -> Just sp
    _ -> Nothing

tick :: Text -> Text
tick n = "`" <> n <> "`"