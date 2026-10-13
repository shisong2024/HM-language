{-# LANGUAGE LambdaCase, FlexibleContexts, TupleSections, OverloadedStrings, ConstraintKinds #-}
module Interp.TypeCheck 
    ( typeChecker
    , typeOfExprWE
    , programChecker
    , sccChecker
    , buildDEnv
    , buildProgramDEnv
    , mergeDEnv
    , candidateHeadsOverlap
    ) where

import Interp.Eval
import Interp.Types
import Interp.Parser (dataDeclsOf)

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad (foldM, forM, forM_, when)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State (get)
import Control.Monad.Reader (MonadReader (ask), local)

import Data.Set (Set)
import Data.Graph (stronglyConnComp)
import Data.Functor ((<&>))
import Data.Text (Text)

import Interp.TypeCheck.Common
import Interp.TypeCheck.Solver
import Interp.TypeCheck.Declarations
import Interp.TypeCheck.Elaboration
import Interp.TypeCheck.Implicit
import Interp.TypeCheck.Infer
import Interp.Pretty (tick)

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
programChecker outT outI p = withInferRollback $ do
    old <- ask
    forM_ (dataDeclsOf p) $ \d -> when (M.member (dName d) (denvDatas old)) $ do
        let newAbility = dName d `S.member` S.fromList [dName a | StmtAbility a <- p]
        when (newAbility /= S.member (dName d) (denvAbilities old)) $
            throwError $ Located Nothing $ DeclRoleChanged (dName d)
    case fstDuplicate [n | StmtEff n <- p] of
        Just n -> throwError $ Located Nothing $ DuplicateEff n
        Nothing -> return ()
    let denv = mergeDEnv (buildProgramDEnv p) old
    local (const denv) $ do
        mapM_ (checkEffTypes denv) [t | StmtType _ _ t <- p]
        programCheckerCore outT outI p

programCheckerCore :: Checker m => TEnv -> IFrame -> Program -> m CProgram
programCheckerCore outerT outerI program = do
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
            forM_ bs2 $ \b -> validateCandidate (cName b) (cScheme b)
            let impT = M.fromList [(cName b, cScheme b) | b <- bs2]
            (_, bs3, res3, fd3) <- sccCheckerElab (M.union impT outer) (impNames: [outerI]) ordinaryDecls
            return (bs2, bs3, impT, M.union res2 res3, fd3)

    let allBindings = impBingdings <> ordBindings
        newT = M.union impT (M.fromList [(cName b, cScheme b) | b <- ordBindings])
        env = M.union newT outer
        scope = impNames: [outerI]
    denv <- ask
    forM_ allBindings $ \b -> withNote ("in definition of " <> tick (cName b)) $
        mapM_ (checkAbilityType denv . predType) (sPreds (cScheme b))
    exprRes <- forM [(i, e) | (i, StmtExpr e) <- zip [0..] program] $ \(i, e) -> do
        let blocked = [n | n <- S.toList (freeVars e), S.member n failed]
        case blocked of
            n: _ -> return (i, Left $ Located Nothing $ CalleeNotChecked n)
            [] -> do
                r <- tryInfer $ do
                    rr <- inferExpr env scope e
                    let ws = map (applyWanted (irSub rr)) (irWanted rr)
                    solved <- forM ws $ \w -> resolveWanted env w <&> (wtdHole w,)
                    e' <- case fillHoles (M.fromList solved) (irExpr rr) of
                        Left h -> throwError $ Located Nothing $ EscapedImpHole h
                        Right x -> return x
                    validateEffects
                    get >>= \st -> return
                        ( freezeType st (apply (irSub rr) (irType rr)), e'
                        , resolveEff st (applyEff (irSub rr) (irNow rr)))
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
        exprByIndex = M.fromList [(i, e) | (i, Right (_, e, _)) <- exprRes]
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
            [(i, TyExpr (fmap (\(j, _, _) -> j) r)) | (i, r) <- exprRes]

    return CProgram
        { cSub     = M.empty
        , cTEnv    = M.union ctorEnv newT
        , cTypes   = typeMap
        , cProgram = elaborated
        , cEffects = M.fromList [(i, e) | (i, Right (_, _, e)) <- exprRes]
            <> M.mapMaybe (\case TyDef _ sch -> Just (sInitEff sch); _ -> Nothing) typeMap
        , cIFrame  = impNames
        }

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
            r <- tryInfer (checkOne declMap done ds)
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

candidateHeadsOverlap :: S' -> S' -> Bool
candidateHeadsOverlap (Forall av _ at) (Forall bv _ bt) =
    let rename vs start = M.fromList (zip (S.toList vs) (map TVar [start ..]))
        ar = rename av 1
        br = rename bv (S.size av + 1)
        allowed = S.fromList [1 .. S.size av + S.size bv]
        result = solve allowed M.empty (apply ar at) (apply br bt)
    in case result of Just _ -> True; Nothing -> False
    where
        solve allowed su left right =
            let a = apply su left; b = apply su right
                bind v t = if occursIn v t then Nothing else Just (compose (M.singleton v t) su)
                pairs xs ys = if length xs /= length ys then Nothing
                    else foldM (\acc (x,y) -> solve allowed acc x y) su (zip xs ys)
            in if a == b then Just su else case (a,b) of
                (TVar v,t) | S.member v allowed -> bind v t
                (t,TVar v) | S.member v allowed -> bind v t
                (TList x,TList y) -> solve allowed su x y
                (TTuple xs,TTuple ys) -> pairs xs ys
                (TCon n xs,TCon m ys) | n == m -> pairs xs ys
                (TFunc x e y,TFunc u f v) | e == f -> pairs [x,y] [u,v]
                _ -> Nothing