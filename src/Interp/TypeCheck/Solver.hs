{-# LANGUAGE LambdaCase, FlexibleContexts, OverloadedStrings #-}
module Interp.TypeCheck.Solver where

import Interp.Types

import Control.Monad (foldM, forM_, when, unless)
import Control.Monad.Error.Class (MonadError (throwError, catchError))
import Control.Monad.State (MonadState, get, put, modify)

import Data.Map ((!?))
import Data.Set (Set)

import qualified Data.Map as M
import qualified Data.Set as S

import Interp.TypeCheck.Common
import Data.Bifunctor (Bifunctor(bimap))
import Control.Monad.Reader (ask)
import Data.Maybe (isJust)
import Interp.TypeCheck.Declarations (checkTypeCons)
import Data.List (nub)

fresh :: MonadState Counter m => m T'
fresh = TVar <$> freshHole

freshEff :: MonadState Counter m => m Eff
freshEff = Eff S.empty . S.singleton <$> freshHole

apply :: TSub -> T' -> T'
apply ts = \case
    TInt -> TInt
    TBool -> TBool
    TString -> TString
    TVar i -> case ts !? i of
        Nothing -> TVar i
        Just (TVar j) | j == i -> TVar i
        Just tp -> apply ts tp
    TFunc argt eff rest -> TFunc (apply ts argt) (applyEff ts eff) (apply ts rest)
    TList tp -> TList (apply ts tp)
    TTuple tps -> TTuple (fmap (apply ts) tps)
    TCon n arg -> TCon n (fmap (apply ts) arg)

applyPred :: TSub -> Pred -> Pred
applyPred ts = \case
    Implicit t -> Implicit (apply ts t)
    NameImplicit n t -> NameImplicit n (apply ts t)

applyS' :: TSub -> S' -> S'
applyS' ts sch@(Forall vs ps t) =
    let ts' = foldr M.delete ts vs
    in (
        (Forall vs (map (applyPred ts') ps) (apply ts' t))
            { sInitEff = applyEff ts' (sInitEff sch)
            , sEffCs = [(applyEff ts' a, applyEff ts' b) | (a,b) <- sEffCs sch]
            }
        ) { sDictCall = applyEff ts' (sDictCall sch) }

applyTEnv :: TSub -> TEnv -> TEnv
applyTEnv = M.map . applyS'

applyWanted :: TSub -> Wanted -> Wanted
applyWanted ts w = w { wtdType = apply ts (wtdType w) }

applyInfer :: TSub -> InferRes -> InferRes
applyInfer ts r = r
    { irSub    = compose ts (irSub r)
    , irWanted = map (applyWanted ts) (irWanted r)
    , irType   = apply ts (irType r)
    , irNow    = applyEff ts (irNow r)
    }

applyEff :: TSub -> Eff -> Eff
applyEff ts e = e
    { effVars = S.fromList [num i | i <- S.toList (effVars e)]
    , effParams = M.map (map (map (apply ts))) (effParams e)
    } { effExclusions = M.fromList [(num i, ns) | (i, ns) <- M.toList (effExclusions e)] }
    where
        num i = case M.lookup i ts of 
            Just (TVar j) -> j
            _ -> i

unionsEff :: [Eff] -> Eff
unionsEff = foldr unionEff emptyEff

freshHole :: MonadState Counter m => m HoleId
freshHole = do
    st <- get
    put st {nextId = nextId st + 1}
    return (nextId st)

compose :: TSub -> TSub -> TSub
compose s2 s1 = M.union (M.map (apply s2) s1) s2

unify :: (MonadState Counter m, MonadError (Located TypeError) m) => (T', T') -> m TSub
unify pair = do
    old <- get
    u <- unifyCore $ bimap (apply (effTypeSub old)) (apply (effTypeSub old)) pair
    let update e = if M.null (effParams e) then e else applyEff u e
    unless (M.null u) $ modify (\st -> st 
        { effDefs = foldr (M.adjust update) (effDefs st) (S.toList (effParamRows st))
        , effRels = [(update a, update b) | (a, b) <- effRels st] 
        , effTypeSub = M.union u (effTypeSub st)
        })
    return u

unifyCore :: (MonadState Counter m, MonadError (Located TypeError) m) => (T', T') -> m TSub
unifyCore = \case
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
    (TFunc a1 e1 r1, TFunc a2 e2 r2) -> do
        u <- unifyEffParams e1 e2
        unifyEff (applyEff u e1) (applyEff u e2)
        ts  <- unify (apply u a1, apply u a2)
        ts' <- unify (apply ts r1, apply ts r2)
        return $ compose ts' (compose ts u)
    (t1, t2) -> throwError (Located Nothing $ TypeMismatch t1 t2)

unifyEff :: (MonadState Counter m, MonadError (Located TypeError) m) => Eff -> Eff -> m ()
unifyEff a0 b0 = do
    p <- unifyEffParams a0 b0
    st <- get
    let aa = resolveEff st $ applyEff p a0
        bb = resolveEff st $ applyEff p b0
        common = effLabs aa `S.intersection` effLabs bb
        trim row = row
            { effLabs = effLabs row `S.difference` common
            , effParams = M.withoutKeys (effParams row) common
            }
        a = trim aa
        b = trim bb
        variable e = if S.null (effLabs e) && S.size (effVars e) == 1 && M.null (effExclusions e)
            then Just (S.findMin (effVars e)) else Nothing
        bind v e = modify (\c -> c 
            { effDefs = M.insert v 
                (e { effVars = S.delete v (effVars e), effExclusions = M.delete v (effExclusions e) }) (effDefs c)
            , effParamRows = (if M.null (effParams e) then S.delete else S.insert) v (effParamRows c)})
    if a == b then return ()
    else case (variable a, variable b) of
        (Just v, Just w) -> if v < w then bind w a else bind v b
        (Just v, _) | M.notMember v (effExclusions b) -> bind v b
        (_, Just w) | M.notMember w (effExclusions a) -> bind w a
        _ -> if S.null (effVars a) && S.null (effVars b) then
            throwError $ Located Nothing $ EffMismatch a b
            else modify (\c -> c { effRels = (a, b): effRels c } )

unifyEffParams :: (MonadState Counter m, MonadError (Located TypeError) m) => Eff -> Eff -> m TSub
unifyEffParams a b = do
    (u, _) <- normalizeEff (unionEff a b)
    unless (M.null u) $ modify (\st -> st { effTypeSub = M.union u (effTypeSub st) })
    return u

normalizeEff :: (MonadState Counter m, MonadError (Located TypeError) m) => Eff -> m (TSub, Eff)
normalizeEff e = do
    st <- get
    let row = resolveEff st e
        same su = \case
            [] -> return su
            x: xs -> foldM (\s y -> unify (TTuple (map (apply s) x), TTuple (map (apply s) y)) >>= \u -> return (compose u s)) su xs
    su <- foldM same M.empty (M.elems (effParams row))
    return (su, (applyEff su row) { effParams = M.map (take 1 . map (map (apply su))) (effParams row)})

bindVar :: MonadError (Located TypeError) m => TypeVar -> T' -> m TSub
bindVar i (TVar j)
    | i == j = return M.empty
bindVar i t = if i `occursIn` t then throwError $ Located Nothing OccurCheck else return (M.singleton i t)

resolveEff :: Counter -> Eff -> Eff
resolveEff st initial = canonical (go S.empty initial)
    where
        canonical e =
            let e1 = foldl rewrite e (effRels st)
            in if e1 == e then e else canonical e1
        rewrite e (a, b) =
            let (l, s) = if rank a > rank b then (a, b) else (b, a)
            in  if l /= emptyEff && M.null (effExclusions l) && M.null (effExclusions s) && M.null (effExclusions e) 
                    && effLabs l `S.isSubsetOf` effLabs e && effVars l `S.isSubsetOf` effVars e
                then unionEff s (e 
                    { effLabs = effLabs e `S.difference` effLabs l
                    , effVars = effVars e `S.difference` effVars l
                    , effParams = M.withoutKeys (effParams e) (effLabs l)
                    })
                else e
        rank e = (S.size (effLabs e) + S.size (effVars e), e)
        go seen e = unionEff (e { effVars = S.empty, effExclusions = M.empty })
            (unionsEff [foldr removeEff (one seen v) (S.toList (M.findWithDefault S.empty v (effExclusions e))) | v <- S.toList (effVars e)])
        one seen v
            | S.member v seen = emptyEff
            | otherwise = case M.lookup v (effDefs st) of
                Nothing -> Eff S.empty (S.singleton v)
                Just e -> go (S.insert v seen) e

occursIn :: TypeVar -> T' -> Bool
occursIn i = \case
    TInt -> False
    TBool -> False
    TString -> False
    TVar j -> i == j
    TList tp -> i `occursIn` tp
    TTuple tps -> any (occursIn i) tps
    TFunc at _ rt -> i `occursIn` at || i `occursIn` rt
    TCon _ args -> any (occursIn i) args

instantiate :: MonadState Counter m => S' -> m ([Pred], T')
instantiate sch = instantiateFull sch >>= \(ps, ty, _) -> return (ps, ty)

instantiateFull :: MonadState Counter m => S' -> m ([Pred], T', Eff)
instantiateFull sch@(Forall vs ps tp) = do
    ftvs <- mapM (const fresh) (S.toList vs)
    let su = M.fromList $ zip (S.toList vs) ftvs
    modify $ \c -> c { effRels = [(applyEff su a, applyEff su b) | (a, b) <- sEffCs sch] <> effRels c }
    return (map (applyPred su) ps, apply su tp, applyEff su (sDictCall sch))

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
    TFunc arg e res -> S.unions [ftv arg, ftvEff e, ftv res]
    TCon _ args -> S.unions (fmap ftv args)

ftvPred :: Pred -> Set TypeVar
ftvPred = ftv . predType

ftvPreds :: [Pred] -> Set TypeVar
ftvPreds = S.unions . map ftvPred

ftvS' :: S' -> Set TypeVar
ftvS' sch@(Forall vs ps t) = S.unions
    [ ftv t, ftvPreds ps
    , ftvEff (sDictCall sch)
    , S.unions [ftvEff a `S.union` ftvEff b | (a,b) <- sEffCs sch]
    ] `S.difference` vs

ftvTEnv :: TEnv -> Set TypeVar
ftvTEnv = S.unions . map ftvS' . M.elems

ftvEff :: Eff -> Set Int
ftvEff e = effVars e `S.union` S.unions (map ftv (concat (concat (M.elems (effParams e)))))

freezeType :: Counter -> T' -> T'
freezeType st = go . apply (effTypeSub st)
    where
        go = \case
            TFunc a e b -> TFunc (go a) (resolveEff st e) (go b)
            TList t -> TList $ go t
            TTuple ts -> TTuple $ map go ts
            TCon n ts -> TCon n $ map go ts
            t -> t

checkEff :: (MonadState Counter m, MonadError (Located TypeError) m) => Eff -> Eff -> m ()
checkEff a0 b0 = do
    st <- get
    let a = resolveEff st a0
        b = resolveEff st b0
    if S.null (effVars b) then do
        if effLabs a `S.isSubsetOf` effLabs b then return ()
        else throwError $ Located Nothing $ EffMismatch a b
        forM_ (S.toList (effVars a)) $ \v -> modify (\c -> c { effDefs = M.insert v b (effDefs c) })
    else if a == b then do
        let rest = a { effLabs = effLabs a `S.difference` effLabs b, effParams = M.withoutKeys (effParams a) (effLabs b) }
            vars = S.toAscList (effVars b)
        case vars of
            v: vs -> do
                unifyEff (Eff S.empty (S.singleton v)) rest
                forM_ vs $ \w -> unifyEff (Eff S.empty (S.singleton w)) emptyEff
            [] -> return ()
    else unifyEff (unionEff a (b { effVars = S.empty, effExclusions = M.empty })) b

compatible :: (MonadState Counter m, MonadError (Located TypeError) m) => T' -> T' -> m TSub
compatible act expe = case (act, expe) of
    (TFunc aa ae ar, TFunc ea ee er) -> do
        u <- compatible ea aa
        p <- unifyEffParams (applyEff u ae) (applyEff u ee)
        let s = compose p u
        checkEff (applyEff s ae) (applyEff s ee)
        v <- compatible (apply s ar) (apply s er)
        return (compose v s)
    _ -> unify (act, expe)

applyFunction :: (MonadState Counter m, MonadError (Located TypeError) m) => T' -> T' -> Eff -> T' -> m TSub
applyFunction f a e out = case f of
    TFunc expected latent res -> do
        u <- compatible a expected
        p <- unifyEffParams (applyEff u latent) (applyEff u e)
        let s = compose p u
        unifyEff (applyEff s latent) (applyEff s e)
        v <- unify (apply s res, apply s out)
        return (compose v s)
    TVar _ -> unify (f, TFunc a e out)
    _ -> unify (f, pureFunc a out)

operationSchemes :: EffDecl -> TEnv
operationSchemes d = M.fromList [(n, Forall (ftv ty) [] ty) | (n, t) <- edOperations d,
    let ty = case t of
            TFunc a e b -> TFunc a (unionEff e (EffRow (S.singleton (edName d)) S.empty 
                (if null (edParams d) then M.empty else M.singleton (edName d) [[TVar i | i <- [0..length (edParams d) - 1]]]) 
                M.empty)) b
            _ -> t
    ]

validateEffDecl :: Checker m => EffDecl -> m ()
validateEffDecl d = do
    denv <- ask
    when (edName d `elem` ["Console", "FileIO"]) $ reject "host operation interfaces cannot be redifined"
    when (null $ edOperations d) $ reject "effects requires operations"
    when (isJust $ fstDuplicate (map fst (edOperations d))) $ reject "duplicate operation"
    forM_ (edOperations d) $ \(_, ty) -> case ty of
        TFunc {} -> checkTypeCons denv ty
        _ -> reject "operation must have a function type"

joinTypes :: (MonadState Counter m, MonadError (Located TypeError) m) => T' -> T' -> m (TSub, T')
joinTypes a b = case (a, b) of
    (TFunc aa ae ar, TFunc ba be br) -> do
        u <- unify (aa, ba)
        (v, r) <- joinTypes (apply u ar) (apply u br)
        return (compose v u, TFunc (apply v (apply u aa)) (unionEff ae be) r)
    _ -> unify (a, b) >>= \u -> return (u, apply u a)


relevantRelations :: Counter -> Set Int -> [(Eff, Eff)]
relevantRelations st vs = nub [(a, b) 
    | (a0, b0) <- effRels st
    , let a = resolveEff st a0, let b = resolveEff st b0, a /= b
    , not (S.null (S.intersection vs (S.union (effVars a) (effVars b))))
    ]

typeEffectVars :: T' -> Set Int
typeEffectVars = \case
    TFunc a e b -> S.unions [typeEffectVars a, effVars e, typeEffectVars b]
    TList t -> typeEffectVars t
    TTuple ts -> S.unions $ map typeEffectVars ts
    TCon _ ts -> S.unions $ map typeEffectVars ts
    _ -> S.empty

inputEffectVars :: T' -> Set Int
inputEffectVars = \case
    TFunc a _ b -> typeEffectVars a `S.union` inputEffectVars b
    TList t -> inputEffectVars t
    TTuple ts -> S.unions $ map inputEffectVars ts
    TCon _ ts -> S.unions $ map inputEffectVars ts
    _ -> S.empty

relationClosure :: Counter -> Set Int -> Set Int
relationClosure st start =
    let next = S.unions
            ( start: [ effVars a `S.union` effVars b
            | (a0, b0) <- effRels st <> [(Eff S.empty (S.singleton v), e) | (v, e) <- M.toList (effDefs st)]
            , let a = a0, let b = b0
            , not (S.null (S.intersection start (effVars a `S.union` effVars b)))
            ])
    in if next == start then start else relationClosure st next

validateEffects :: (MonadState Counter m, MonadError (Located TypeError) m) => m ()
validateEffects = do
    before <- get
    forM_ (nub $ effRels before) $ \(a0, b0) -> do
        _ <- unifyEffParams a0 b0
        st <- get
        let a = resolveEff st a0
            b = resolveEff st b0
        when (a /= b && S.size (effVars a) <= 1 || S.size (effVars b) <= 1) $ unifyEff a b
        when (S.null (effVars a) && S.null (effVars b) && a /= b) $
            throwError $ Located Nothing $ EffMismatch a b

withInferRollback :: (MonadState Counter m, MonadError (Located TypeError) m) => m a -> m a
withInferRollback act = do
    before <- get
    act `catchError` \err -> do
        after <- get
        put before { nextId = nextId after }
        throwError err

tryInfer :: (MonadState Counter m, MonadError (Located TypeError) m) => m a -> m (Either (Located TypeError) a)
tryInfer act = (Right <$> withInferRollback act) `catchError` (return . Left)
