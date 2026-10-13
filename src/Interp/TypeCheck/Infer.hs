{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Interp.TypeCheck.Infer where

import Interp.Types

import qualified Data.Set as S

import Interp.Eval (addSpan, stripAt, freeVars, paramName)

import Control.Monad.State (get, modify)
import Control.Monad.Except (throwError, catchError, MonadError)
import Control.Monad (forM_, foldM, forM)
import Control.Monad.Reader (ask)

import Data.Text (Text, pack)
import Data.Map ((!?))
import Data.List (nub)

import qualified Data.Map as M

import Interp.TypeCheck.Declarations (checkTypeCons, checkAbilityType)
import Interp.TypeCheck.Elaboration
import Interp.TypeCheck.Pattern (patType, checkDupPatVar, exhaustive)
import Interp.TypeCheck.Solver
import Interp.TypeCheck.Common
import Interp.TypeCheck.Implicit (groupWanted, deferWanted, resolveWanted)
import Interp.Pretty (tick)

typeChecker :: Checker m => TEnv -> E' -> m (TSub, T')
typeChecker env e = do
    r <- inferExpr env [] e
    case irWanted r of
        [] -> get >>= \st -> return (irSub r, freezeType st $ irType r)
        w: _ -> throwError $ Located (wtdSpan w)
            (AmbiguousImpType (apply (irSub r) (wtdType w)))

typeOfExprWE :: Checker m => TEnv -> IEnv -> E' -> m (S', Eff)
typeOfExprWE outT outI e = withInferRollback $ typeOfExprWECore outT outI e

typeOfExprWECore :: Checker m => TEnv -> IEnv -> E' -> m (S', Eff)
typeOfExprWECore env ienv e = do
    r <- inferExpr env ienv e
    (sch, _) <- closeBinding env (irType r) (irWanted r) (irExpr r) (irNow r)
    denv <- ask
    mapM_ (checkAbilityType denv . predType) (sPreds sch)
    get >>= \st -> return (sch, resolveEff st (applyEff (irSub r) (irNow r)))

inferExpr :: Checker m => TEnv -> IEnv -> E' -> m InferRes
inferExpr env ienv e = case e of
    ILit _ -> return $ pureInfer e TInt

    BLit _ -> return $ pureInfer e TBool

    SLit _ -> return $ pureInfer e TString

    Var t -> case env !? t of
        Nothing -> throwError (Located Nothing $ UnboundVar t)
        Just sch  -> do
            (preds, ty, callEff) <- instantiateFull sch
            denv <- ask
            forM_ preds $ \p -> case predType p of
                TVar _ -> return ()
                ty' -> checkAbilityType denv ty'
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
            return $ InferRes M.empty ws ty e' callEff

    ListLit es -> case es of
        [] -> pureInfer e . TList <$> fresh
        e0: rest -> do
            r0 <- inferExpr env ienv e0
            let step (su, ws, elemTy, es', now) ex = do
                    r <- inferExpr (applyTEnv su env) ienv ex
                    u <- unify (apply (irSub r) (apply su elemTy), apply (irSub r) (irType r))
                    let su' = compose u (compose (irSub r) su)
                    return (su', ws <> irWanted r, elemTy, irExpr r: es', unionEff now (irNow r))
            (sub, wanted, ty, revE, now) <-
                foldM step (irSub r0, irWanted r0, irType r0, [irExpr r0], irNow r0) rest
            return $ InferRes
                { irSub    = sub
                , irWanted = map (applyWanted sub) wanted
                , irType   = TList (apply sub ty)
                , irExpr   = ListLit (reverse revE)
                , irNow    = applyEff sub now
                }

    TupleLit es -> do
        let step (su, ws, tys, done, now) ex = do
                r <- inferExpr (applyTEnv su env) ienv ex
                let su' = compose (irSub r) su
                return (su', ws <> irWanted r, tys <> [irType r], done <> [irExpr r], unionEff now (irNow r))
        (su, ws, tys, es', now) <- foldM step (M.empty, [], [], [], emptyEff) es
        return $ InferRes
            { irSub    = su
            , irWanted = map (applyWanted su) ws
            , irType   = TTuple (map (apply su) tys)
            , irExpr   = TupleLit es'
            , irNow    = applyEff su now
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
                    (applyEff su (unionEff (irNow ra) (irNow rb)))
            _ -> do
                u1 <- unify (apply su0 (irType ra), TInt)
                u2 <- unify (apply u1 (apply su0 (irType rb)), TInt)
                let su = compose u2 (compose u1 su0)
                    out = case op of
                        OArith _ -> TInt
                        OCmp _   -> TBool
                return $ InferRes su (map (applyWanted su) (irWanted ra <> irWanted rb))
                    out (BOpr op (irExpr ra) (irExpr rb))
                    (applyEff su (unionEff (irNow ra) (irNow rb)))

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
        (u3, joined) <- joinTypes (apply su2 (irType ry)) (apply su2 (irType rn))
        let su = compose u3 su2
        return $ InferRes su (map (applyWanted su) (irWanted rc <> irWanted ry <> irWanted rn))
            (apply su joined) (If (irExpr rc) (irExpr ry) (irExpr rn))
            (applyEff su (unionsEff [irNow rc, irNow ry, irNow rn]))

    Lambda n body -> do
        tv <- fresh
        let env' = M.insert n (Forall S.empty [] tv) env
        r <- inferExpr env' ienv body
        return $ r { irType = TFunc (apply (irSub r) tv) (irNow r) (irType r), irExpr = Lambda n (irExpr r), irNow = emptyEff }

    App f arg -> do
        rf <- inferExpr env ienv f
        ra <- inferExpr (applyTEnv (irSub rf) env) ienv arg
        out <- fresh
        eff <- freshEff
        let su0 = compose (irSub ra) (irSub rf)
        u <- applyFunction (apply su0 (irType rf)) (apply su0 (irType ra)) eff out
            `catchError` calleeNote (applyTEnv su0 env) f
        let su = compose u su0
        return $ InferRes su (map (applyWanted su) (irWanted rf <> irWanted ra))
            (apply su out) (App (irExpr rf) (irExpr ra))
            (applyEff su (unionsEff [irNow rf, irNow ra, eff]))


    Match scrut arms -> do
        rs <- inferExpr env ienv scrut
        (su, ws, out, arms', armNow) <- foldM (inferArm env ienv (irType rs))
            (irSub rs, irWanted rs, Nothing, [], emptyEff) arms
        denv <- ask
        checkNonExhaustive denv (apply su (irType rs)) arms
        outTy <- maybe fresh (return . apply su) out
        return $ InferRes su (map (applyWanted su) ws) outTy (Match (irExpr rs) arms')
            (applyEff su (unionEff (irNow rs) armNow))

    AnnT ex ann -> do
        denv <- ask
        checkTypeCons denv ann
        r <- inferExpr env ienv ex
        let vs = filter (< 0) $ S.toList (ftv ann)
        fs <- mapM (const fresh) vs
        let ann' = apply (M.fromList (zip vs fs)) ann
        u <- compatible (apply (irSub r) (irType r)) (apply (irSub r) ann')
        return (applyInfer u r) { irExpr = AnnT (irExpr r) ann, irType = apply u ann' }

    At sp ex -> do
        r <- inferExpr env ienv ex `catchError` (throwError . addSpan sp)
        return r { irWanted = map (underSpan sp) (irWanted r), irExpr = At sp (irExpr r) }

    ImpHole h -> throwError (Located Nothing (EscapedImpHole h))


inferLet :: Checker m => TEnv -> IEnv -> Text -> E' -> E' -> m InferRes
inferLet env ienv n rhs body = do
    (su, sch, rhsIR, initNow) <- case stripAt rhs of
        Lambda {} -> do
            (sub, bds) <- inferBindingGroupWithNotes False env ienv [Def n [] rhs]
            case bds of
                [(_, ty, e)] -> return (sub, ty, e, emptyEff)
                _ -> throwError $ Located Nothing DictConstrainDidNotConverge
        _ -> do
            rr <- inferExpr env ienv rhs
            let env1 = applyTEnv (irSub rr) env
            (ty, e) <- closeBinding env1 (irType rr) (irWanted rr) (irExpr rr) (irNow rr)
            return (irSub rr, ty, e, if null (sPreds ty) then irNow rr else emptyEff)
    rb <- inferExpr (M.insert n sch (applyTEnv su env)) ienv body
    return $ rb
        { irSub  = compose (irSub rb) su
        , irExpr = Let n rhsIR (irExpr rb)
        , irNow  = unionEff initNow (irNow rb)
        }

inferArm :: Checker m => TEnv -> IEnv -> T' -> (TSub, [Wanted], Maybe T', [(P', E')], Eff) -> (P', E') -> m (TSub, [Wanted], Maybe T', [(P', E')], Eff)
inferArm env ienv scrutTy (su, ws, oldout, done, now) (pat, body) = do
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
            (u, joined) <- joinTypes (apply su2 t) (apply su2 (irType rb))
            return (compose u su2, apply u joined)
    return (su3, ws <> irWanted rb, Just out, done <> [(pat, irExpr rb)], unionEff now (irNow rb))

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
    (su, rhsWs, sols, bds, dictNow) <- foldM inferDict (irSub appRes, [], M.empty, [], emptyEff) chosen
    let exHoles = S.fromList (map (dsHole . snd) chosen)
        remaining = filter (not . (`S.member` exHoles) . wtdHole) (irWanted appRes)
        body = replaceSolvedHoles sols (irExpr appRes)
        lowered = Let headT headE (foldr (\(n, e) b -> Let n e b) body bds)
    return $ InferRes
        { irSub    = su
        , irWanted = map (applyWanted su) (remaining <> rhsWs)
        , irType   = apply su (irType appRes)
        , irExpr   = lowered
        , irNow    = applyEff su (unionEff (irNow appRes) dictNow)
        }
    where
        dictSpan n = case [daSpan d | d <- dicts, daName d == n] of
            sp: _ -> Just sp
            _ -> Nothing

        inferArg acc arg = do
            ra <- inferExpr (applyTEnv (irSub acc) env) ienv arg
            out <- fresh
            eff <- freshEff
            let su0 = compose (irSub ra) (irSub acc)
            u <- applyFunction (apply su0 (irType acc)) (apply su0 (irType ra)) eff out
                `catchError` calleeNote (applyTEnv su0 env) head0
            let su = compose u su0
            return $ InferRes
                { irSub    = su
                , irWanted = map (applyWanted su) (irWanted acc <> irWanted ra)
                , irType   = apply su out
                , irExpr   = App (irExpr acc) (irExpr ra)
                , irNow    = applyEff su (unionsEff [irNow acc, irNow ra, eff])
                }

        inferDict (su, ws, solved, bs, now) (d, slot) = do
            rd <- inferExpr (applyTEnv su env) ienv (daExpr d)
            let su0 = compose (irSub rd) su
                actual = apply su0 (irType rd)
                expected = apply su0 (dsType slot)
            u <- unify (actual, expected) `catchError` \(Located _ reason) ->
                throwError $ Located (Just (daSpan d)) (DictArgMismatch (daName d) actual expected reason)
            tmp <- freshInternal "dict"
            return (compose u su0, ws <> irWanted rd,
                M.insert (dsHole slot) (Var tmp) solved, bs <> [(tmp, irExpr rd)], unionEff  now (irNow rd))

checkNonExhaustive :: MonadError (Located TypeError) m => DEnv -> T' -> [(P', E')] -> m ()
checkNonExhaustive denv t arms
    | exhaustive denv t (map fst arms) = return ()
    | otherwise = throwError $ Located Nothing $ NonExhaustivePat t

inferBindingGroup :: Checker m => TEnv -> [IFrame] -> [Decl] -> m (TSub, [(Text, S', E')])
inferBindingGroup = inferBindingGroupWithNotes True

inferBindingGroupWithNotes :: Checker m => Bool -> TEnv -> [IFrame] -> [Decl] -> m (TSub, [(Text, S', E')])
inferBindingGroupWithNotes annoate outer ienv ds = do
    prepared <- mapM prepareDecl ds
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
                        (sch, body) <- note n $ closeBinding (applyTEnv finalSu outer) (irType r) (irWanted r) (irExpr r) (irNow r)
                        return (n, sch, body)
                    return (finalSu, checked)

        inferOne renv (sub, done) (Def n ps b, ty) = do
            let ps' = map (\case ImplicitParam x t -> ImplicitParam x (apply sub t); p -> p) ps
            r <- note n $ inferDeclBodyPrepared (applyTEnv sub renv) ienv ps' (mapAnnotations (apply sub) True b)
            let su0 = compose (irSub r) sub
            u <- unify (apply su0 ty, apply su0 (irType r))
            return (compose u su0, (n, r): done)

closeBinding :: Checker m => TEnv -> T' -> [Wanted] -> E' -> Eff -> m (S', E')
closeBinding outer ty0 wanted0 body now = do
    initial <- get
    let provisional = freezeType initial ty0
        anchored = inputEffectVars provisional `S.union` ftvTEnv outer
        lvs = typeEffectVars provisional `S.union` effVars (resolveEff initial now)
    forM_ (S.toList (lvs `S.difference` anchored)) $ \v ->
        modify (\c -> c { effectDefs = M.insert v emptyEff (effectDefs c) })
    validateEffects
    st <- get
    let ty = freezeType st ty0
        callNow = resolveEff st now
        seedVars = S.unions [ftv ty, effVars callNow, S.unions (map (ftv . wtdType) wanted0)]
            `S.difference` ftvTEnv outer
        genVars = relationClosure st seedVars `S.difference` ftvTEnv outer

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
        solved = M.fromList ([(h, c) | (_, h, c) <- concreteSolutions] <> hiddenSolutions)
        predicates = [pred' | (pred', _, _, _) <- hidden]
        captured = [(n, c) | (Just n, _, c) <- concreteSolutions]

    body' <- case fillHoles solved body of
        Left h  -> throwError $ case [w | w <- concreteWs, wtdHole w == h] of
            w: _ -> Located (wtdSpan w) (EscapedImpHole h)
            [] -> Located Nothing (EscapedImpHole h)
        Right e -> return e

    let bodyB = foldr (\(n, c) b -> App (Lambda n b) c) body' captured
        aliasOne (_, pri, aliases, _) b = foldr (\x acc -> Let x (Var pri) acc) b aliases
        body'' = foldr (Lambda . (\(_, n, _, _) -> n)) (foldr aliasOne bodyB hidden) hidden
        scheme = (Forall genVars predicates ty)
            { sDictCall = if null hidden then emptyEff else callNow
            , sEffCs    = relevantRelations st genVars
            , sInitEff  = if null hidden then callNow else emptyEff
            }
    return (scheme, body'')

inferDeclBodyPrepared :: Checker m => TEnv -> IEnv -> [Param] -> E' -> m InferRes
inferDeclBodyPrepared env ienv params body = go env params
    where
        go current = \case
            [] -> inferExpr current ienv body

            (ExplicitParam n : rest) -> do
                a <- fresh
                r <- go (M.insert n (Forall S.empty [] a) current) rest
                return r
                    { irType = TFunc (apply (irSub r) a) (irNow r) (irType r)
                    , irExpr = Lambda n (irExpr r)
                    , irNow  = emptyEff
                    }
            (ImplicitParam n annotation : rest) -> do
                denv <- ask
                checkAbilityType denv annotation
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
