{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Interp.TypeCheck.Infer where

import Interp.Types
import Interp.Builtin (consoleEffDecl, fileIOEffDecl)

import qualified Data.Set as S

import Interp.Eval (addSpan, stripAt, freeVars, paramName)

import Control.Monad.State (get, modify, MonadState)
import Control.Monad.Except (throwError, catchError, MonadError)
import Control.Monad (forM_, foldM, forM, unless, when)
import Control.Monad.Reader (ask)

import Data.Text (Text, pack, stripPrefix, splitOn)
import Data.Map ((!?))
import Data.List (nub, partition)

import qualified Data.Map as M

import Interp.TypeCheck.Declarations (checkTypeCons, checkAbilityType, getterAbility, getterFieldOf, setterAbility, setterFieldOf)
import Interp.TypeCheck.Elaboration
import Interp.TypeCheck.Pattern (patType, checkDupPatVar, exhaustive)
import Interp.TypeCheck.Solver
import Interp.TypeCheck.Common
import Interp.TypeCheck.Implicit (groupWanted, nameGroups, deferWanted, resolveWanted)
import Interp.Pretty (tick)
import Data.Maybe (fromMaybe)
import Data.Set (Set)

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
    -- Inspecting a binding is not defining a wrapper around it: keep its
    -- public slots, including distinct explicitly named slots of one type.
    let wanted = case stripAt e of
            Var _ -> [w { wtdBind = wtdName w } | w <- irWanted r]
            _ -> irWanted r
    (sch, _) <- closeBinding env (irType r) wanted (irExpr r) (irNow r)
    denv <- ask
    mapM_ (checkAbilityType denv . predType) (sPreds sch)
    get >>= \st -> return (sch, resolveEff st (applyEff (irSub r) (irNow r)))

inferExpr :: Checker m => TEnv -> IEnv -> E' -> m InferRes
inferExpr env ienv e = do
    r <- inferExprCore env ienv e
    r1 <- resolveFieldWanted r
    (u, now) <- normalizeEff (irNow r1)
    return (applyInfer u r1) { irNow = applyEff u now }

inferExprCore :: Checker m => TEnv -> IEnv -> E' -> m InferRes
inferExprCore env ienv e = case e of
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
                    , wtdSearch = predSearchName p
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

    StrictLet n rhs body -> do
        rr <- inferExpr env ienv rhs
        let env1 = applyTEnv (irSub rr) env
            wantedVs = S.unions (map (ftv . wtdType) (irWanted rr))
            ordinarySch = if irNow rr == emptyEff then
                Forall (ftv (irType rr) `S.difference` (ftvTEnv env1 `S.union` wantedVs)) [] (irType rr)
                else Forall S.empty [] (irType rr)
        (sch, rhsIR, rhsWanted) <- case stripAt rhs of
            Lambda {} -> do
                let (outerWs,localWs) = partition (\w -> not (S.null (ftv (wtdType w) `S.intersection` ftvTEnv env1))) (irWanted rr)
                (s,b) <- closeBindingWithOpen (S.fromList (map wtdHole outerWs)) env1 (irType rr) localWs (irExpr rr) emptyEff
                return (s,b,outerWs)
            _ -> return (ordinarySch,irExpr rr,irWanted rr)
        rb <- inferExpr (M.insert n sch env1) ienv body
        let su = compose (irSub rb) (irSub rr)
        return $ InferRes su (map (applyWanted su) (rhsWanted <> irWanted rb))
            (apply su (irType rb)) (StrictLet n rhsIR (irExpr rb))
            (applyEff su (unionEff (irNow rr) (irNow rb)))

    Handle body n ts ret retBody ops -> inferHandler env ienv body n ts ret retBody ops

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

    App f arg -> case headMarker f of
        Just (sp, n)
            | Just fld <- stripPrefix "$fld@" n -> inferFieldAccess env ienv sp fld arg
            | Just fs  <- stripPrefix "$upd@" n -> inferFieldUpdate env ienv sp fs arg
        _ -> do
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

-- | 剥掉外层 @At@ 拿到 head 变量名与位置，用来识别 parse 期留下的字段 marker。
headMarker :: E' -> Maybe (Maybe Span, Text)
headMarker = go Nothing
  where
    go sp (At s e) = go (Just (fromMaybe s sp)) e
    go sp (Var n)  = Just (sp, n)
    go _  _        = Nothing

recVal, recOther, fieldErr :: Text
recVal   = "$recVal"
recOther = "$recOther"
fieldErr = "$fieldErr"

-- | 字段相关错误：带上 marker 的源码位置，用专用的 @FieldError@ 而非 @HandleError@。
fieldReject :: MonadError (Located TypeError) m => Maybe Span -> Text -> m a
fieldReject sp msg = throwError (Located sp (FieldError msg))

-- | 按 data 名和字段名查出 (构造子, 参数下标, 元数, 是否单构造子)。
dataFieldInfo :: Checker m => Maybe Span -> DEnv -> Text -> Text -> m (Text, Int, Int, Bool)
dataFieldInfo sp denv d fld = case M.lookup d (denvFields denv) >>= M.lookup fld of
    Nothing -> fieldReject sp ("`" <> fld <> "` is not a field of `" <> d <> "`.")
    Just (ctor, idx) -> case M.lookup ctor (denvCtors denv) of
        Nothing -> fieldReject sp ("internal: unknown constructor `" <> ctor
                            <> "` while resolving field `" <> fld <> "`.")
        Just ci -> return
            ( ctor, idx, length (ciArgs ci)
            , maybe True ((== 1) . length) (M.lookup d (denvDatas denv)) )

-- | 纯构造：从记录 scrut 投影出字段 fld（构造子 ctor 的第 idx 位）。非单构造子时带
-- @\$fieldErr@ 兜底臂。Stage 1 的具体访问和 Stage 2 的 getter 字典共用它。
projectionExpr :: Text -> Text -> Int -> Int -> Bool -> E' -> E'
projectionExpr fld ctor idx arity solo scrut =
    Match scrut ((PCtor ctor ps, Var recVal) : [bad | not solo])
  where
    ps  = [if j == idx then PVar recVal else PWild | j <- [0 .. arity - 1]]
    bad = (PVar recOther, App (App (Var fieldErr) (SLit fld)) (Var recOther))

-- | 纯构造：单字段 setter 闭包 @lambda v -> lambda r -> r{fld=v}@（把第 idx 位换成 v，
-- 其余位用旧值重建）。vvar/rvar 必须是新鲜名，避免链式 setter 之间相互遮蔽。
setterExpr :: Text -> Text -> Int -> Int -> Bool -> Text -> Text -> E'
setterExpr fld ctor idx arity solo vvar rvar =
    Lambda vvar (Lambda rvar (Match (Var rvar) ((pat, body) : [bad | not solo])))
  where
    oldAt j = "$recOld" <> pack (show j)
    pat  = PCtor ctor [if j == idx then PWild else PVar (oldAt j) | j <- [0 .. arity - 1]]
    body = foldl App (Var ctor) [if j == idx then Var vvar else Var (oldAt j) | j <- [0 .. arity - 1]]
    bad  = (PVar recOther, App (App (Var fieldErr) (SLit fld)) (Var recOther))

-- | 类型驱动的字段访问 @r.f@：
--   * scrutinee 是具体 @TCon d _@ → 据 d 选出构造子与下标，现场构造投影 @Match@ 再重新
--     推断（重推断让 patType 自动算出实例化后的字段类型，也让兜底臂正常过类型检查）。
--   * scrutinee 是抽象 @TVar rho@ → 生成 getter 约束 `@$get_fld rho alpha@ 和字典洞，
--     body 用 @App (ImpHole h) scrut@；等 rho 在调用点被统一成具体 data 后，由
--     'resolveFieldWanted' 解出 alpha（字段类型）并把洞替换成投影函数（多态投影）。
inferFieldAccess :: Checker m => TEnv -> IEnv -> Maybe Span -> Text -> E' -> m InferRes
inferFieldAccess env ienv sp fld scrut = do
    r <- inferExpr env ienv scrut
    denv <- ask
    case apply (irSub r) (irType r) of
        TCon d _ -> do
            (ctor, idx, arity, solo) <- dataFieldInfo sp denv d fld
            inferExpr env ienv (maybe id At sp (projectionExpr fld ctor idx arity solo scrut))
        TVar rho -> do
            alpha <- fresh
            h <- freshHole
            let w = Wanted
                    { wtdHole  = h
                    , wtdType  = TCon (getterAbility fld) [TVar rho, alpha]
                    , wtdScope = ienv
                    , wtdSpan  = sp
                    , wtdName  = Nothing
                    , wtdBind  = Nothing
                    , wtdSearch = Nothing
                    }
            return $ r { irWanted = irWanted r <> [w]
                       , irType   = alpha
                       , irExpr   = App (ImpHole h) (irExpr r) }
        _ -> fieldReject sp ("field `" <> fld <> "` is not a field of a non-record type.")

-- | 类型驱动的字段更新 @r { f = v, ... }@：同样要求具体 @TCon d _@，所有被更新字段须属于
-- 同一构造子；构造出「保留未更新位、替换更新位」的 @Match@ 再重新推断。
inferFieldUpdate :: Checker m => TEnv -> IEnv -> Maybe Span -> Text -> E' -> m InferRes
inferFieldUpdate env ienv sp flds arg = case arg of
    TupleLit (scrut : vals) -> do
        r <- inferExpr env ienv scrut
        denv <- ask
        let names = splitOn "," flds
        case apply (irSub r) (irType r) of
            TCon d _
                | length names /= length vals -> fieldReject sp "malformed record update."
                | (dup : _) <- dupNames names ->
                    fieldReject sp ("field `" <> dup <> "` is given twice in a record update.")
                | otherwise -> do
                    fis <- mapM (\f -> (,) f <$> dataFieldInfo sp denv d f) names
                    let (ctor0, _, arity, solo) = snd (head fis)
                    unless (all (\(_, (c, _, _, _)) -> c == ctor0) fis) $
                        fieldReject sp "a record update cannot mix fields of different constructors."
                    let idxs   = [i | (_, (_, i, _, _)) <- fis]
                        oldAt j = "$recOld" <> pack (show j)
                        valAt j = case [v | ((_, (_, i, _, _)), v) <- zip fis vals, i == j] of
                            (v : _) -> v
                            []      -> Var (oldAt j)
                        pat  = PCtor ctor0
                            [if j `elem` idxs then PWild else PVar (oldAt j) | j <- [0 .. arity - 1]]
                        body = foldl App (Var ctor0) [valAt j | j <- [0 .. arity - 1]]
                        bad  = (PVar recOther, App (App (Var fieldErr) (SLit (head names))) (Var recOther))
                        upd  = Match scrut ((pat, body) : [bad | not solo])
                    inferExpr env ienv (maybe id At sp upd)
            TVar rho
                | length names /= length vals -> fieldReject sp "malformed record update."
                | (dup : _) <- dupNames names ->
                    fieldReject sp ("field `" <> dup <> "` is given twice in a record update.")
                | otherwise -> do
                    -- 多态更新：scrutinee 抽象。为每个字段生成 setter 约束 `$set_f rho beta`
                    -- 和字典洞，链式拼成 `setx vx (sety vy r)`。rho 具体化后由 resolveFieldWanted
                    -- 解出各 beta 并替换成 setter 闭包。
                    (vsub, revs) <- foldVals (irSub r) vals
                    hs <- mapM (const freshHole) names
                    let ves  = map irExpr revs
                        vtys = map (apply vsub . irType) revs
                        rho' = apply vsub (TVar rho)
                        ws = [ Wanted
                                 { wtdHole = h, wtdType = TCon (setterAbility f) [rho', vty]
                                 , wtdScope = ienv, wtdSpan = sp, wtdName = Nothing
                                 , wtdBind = Nothing, wtdSearch = Nothing }
                             | (h, f, vty) <- zip3 hs names vtys ]
                        inner = foldr (\(h, ve) acc -> App (App (ImpHole h) ve) acc)
                                      (irExpr r) (zip hs ves)
                        wanted = map (applyWanted vsub) (irWanted r <> concatMap irWanted revs) <> ws
                        nowEff = applyEff vsub (unionsEff (irNow r : map irNow revs))
                    return $ InferRes vsub wanted rho' inner nowEff
            _ -> fieldReject sp ("cannot update a field of a non-record type.")
    _ -> fieldReject sp "malformed record update."
  where
    foldVals su0 es = foldM step (su0, []) es
      where
        step (su, acc) e0 = do
            rv <- inferExpr (applyTEnv su env) ienv e0
            return (compose (irSub rv) su, acc <> [rv])

dupNames :: [Text] -> [Text]
dupNames xs = [x | (i, x) <- zip [0 :: Int ..] xs, x `elem` drop (i + 1) xs]

-- | 字段约束的「即时解析」：字段多态的约束 `$get_x rho alpha` 带一个**输出**类型
-- alpha（字段类型），而现有隐式搜索只解闭合的输入约束、不解输出变量。所以在推断流里，
-- 一旦记录类型 rho 被统一成具体 data（@TCon d args@），就立刻：查出字段类型 FT、把
-- alpha 统一成 FT、按 d 现场构造投影函数并替换掉字典洞、丢弃该约束。仍未具体（rho 抽象）
-- 的字段约束则原样保留，交给 closeBinding 泛化成隐藏字典参数（多态）。
resolveFieldWanted :: Checker m => InferRes -> m InferRes
resolveFieldWanted r = do
    denv <- ask
    (su, sols, solved) <- foldM (go denv) (irSub r, [], S.empty) (irWanted r)
    if null sols
        then return r
        else do
            let kept = [applyWanted su w | w <- irWanted r, not (S.member (wtdHole w) solved)]
                r' = applyInfer su r
            return r' { irWanted = kept
                      , irExpr = replaceSolvedHoles (M.fromList sols) (irExpr r') }
  where
    go denv (su, sols, solved) w = case apply su (wtdType w) of
        TCon n [recTy, outTy]
            | Just fld <- getterFieldOf n
            , TCon d args <- recTy -> do
                (ctor, idx, arity, solo) <- dataFieldInfo (wtdSpan w) denv d fld
                case M.lookup ctor (denvCtors denv) of
                    Nothing -> return (su, sols, solved)
                    Just ci -> do
                        let ft = apply (M.fromList (zip [0 :: Int ..] args)) (ciArgs ci !! idx)
                        u <- unify (apply su outTy, ft)
                        gv <- freshInternal "g"
                        let su'  = compose u su
                            dict = Lambda gv (projectionExpr fld ctor idx arity solo (Var gv))
                        return (su', (wtdHole w, dict) : sols, S.insert (wtdHole w) solved)
            | Just fld <- setterFieldOf n
            , TCon d args <- recTy -> do
                (ctor, idx, arity, solo) <- dataFieldInfo (wtdSpan w) denv d fld
                case M.lookup ctor (denvCtors denv) of
                    Nothing -> return (su, sols, solved)
                    Just ci -> do
                        let ft = apply (M.fromList (zip [0 :: Int ..] args)) (ciArgs ci !! idx)
                        u <- unify (apply su outTy, ft)
                        vv <- freshInternal "v"
                        rv <- freshInternal "r"
                        let su'  = compose u su
                            dict = setterExpr fld ctor idx arity solo vv rv
                        return (su', (wtdHole w, dict) : sols, S.insert (wtdHole w) solved)
        _ -> return (su, sols, solved)

inferLet :: Checker m => TEnv -> IEnv -> Text -> E' -> E' -> m InferRes
inferLet env ienv n rhs body = do
    (su, sch, rhsIR, initNow, outward) <- case stripAt rhs of
        Lambda {} | S.member n (freeVars rhs) -> do
            (sub, bds) <- inferBindingGroupWithNotes False env ienv [Def n [] rhs]
            case bds of
                [(_, ty, e)] -> return (sub, ty, e, emptyEff, [])
                _ -> throwError $ Located Nothing DictConstrainDidNotConverge
        _ -> do
            rr <- inferExpr env ienv rhs
            let env1 = applyTEnv (irSub rr) env
                escaping w = not (S.null (ftv (wtdType w) `S.intersection` ftvTEnv env1))
                (otd, localWs) = partition escaping (irWanted rr)
            (ty, e) <- closeBindingWithOpen (S.fromList (map wtdHole otd)) env1 (irType rr) localWs (irExpr rr) (irNow rr)
            return (irSub rr, ty, e, if null (sPreds ty) then irNow rr else emptyEff, otd)
    rb <- inferExpr (M.insert n sch (applyTEnv su env)) ienv body
    return $ rb
        { irSub    = compose (irSub rb) su
        , irWanted = map (applyWanted (irSub rb)) outward <> irWanted rb
        , irExpr   = Let n rhsIR (irExpr rb)
        , irNow    = unionEff initNow (irNow rb)
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

inferHandler :: Checker m => TEnv -> IEnv -> E' -> Text -> [T'] -> Text -> E' -> [(Text,Text,Text,E')] -> m InferRes
inferHandler env ienv body n ts ret retBody arms = do
    denv <- ask
    d <- maybe (case n of
        "Console" -> return consoleEffDecl
        "FileIO" -> return fileIOEffDecl
        _ -> reject ("no operations for effect " <> n)
        ) return (M.lookup n (denvOperations denv))
    unless (length ts == length (edParams d)) $ reject "handle effect arity mismatch"
    mapM_ (checkTypeCons denv) ts
    unless (S.fromList [o | (o, _, _, _) <- arms] == S.fromList (map fst (edOperations d)) && length arms == length (edOperations d)) $
        reject "handler must cover each operation exactly once"
    let vs = S.toList (S.filter (< 0) (S.unions (map ftv ts)))
    fs <- mapM (const fresh) vs
    initial <- get
    let actualTs = map (freezeType initial . apply (M.fromList (zip vs fs))) ts
    rb <- inferExpr env ienv body
    (u0, normalized) <- normalizeEff (irNow rb)
    let declared = EffRow (S.singleton n) S.empty (if null ts then M.empty else M.singleton n [actualTs]) M.empty
    when (not (S.member n (effLabs normalized)) && not (S.null (effVars normalized))) $ do
        tailEff <- freshEff
        let v = S.findMin (effVars normalized)
        unifyEff (Eff S.empty (S.singleton v)) (unionEff declared tailEff)
    stBody <- get
    let bodyNow = resolveEff stBody normalized
    up <- unifyEffParams bodyNow declared
    let su0 = compose up (compose u0 (irSub rb))
        residual = removeEff n (applyEff su0 bodyNow)
    rr <- inferExpr (M.insert ret (Forall S.empty [] (apply su0 (irType rb))) (applyTEnv su0 env)) ienv retBody
    out <- widenAnswer (irType rr)
    resumeEff <- freshEff
    let su1 = compose (irSub rr) su0
        step (su,ws,es,now,rigidNames) (op,x,k,e) = do
            when (x == k) $ reject "operation argument and resume names must differ"
            let source = fromMaybe (error "validated operation") (lookup op (edOperations d))
                paramSub = M.fromList (zip [0..] actualTs)
                ty = apply su (apply paramSub source)
            (arg,res) <- case ty of
                TFunc a _ b -> return (a,b)
                _ -> reject "operation must be a function"
            let generics = ftv ty `S.difference` S.unions (map (ftv . apply su) actualTs)
            rigid <- forM (S.toList generics) $ \i -> do
                h <- freshHole
                return (i,TCon ("$handlerRigid" <> pack (show h)) [])
            let rigidSub = M.fromList rigid
                a = apply rigidSub arg
                b = apply rigidSub res
                clauseEnv = M.insert x (Forall S.empty [] a) $ M.insert k
                    (Forall S.empty [] (TFunc b resumeEff (apply su out))) (applyTEnv su env)
            r <- inferExpr clauseEnv ienv e
            u <- compatible (irType r) (apply (irSub r) (apply su out))
            let sub = compose u (compose (irSub r) su)
            let names = S.fromList [name | (_,TCon name []) <- rigid]
            return (sub,ws <> irWanted r,es <> [(op,x,k,irExpr r)],unionEff now (irNow r),S.union rigidNames names)
    (su, ws, arms', clauseNow, rigidNames) <- foldM step (su1, irWanted rb <> irWanted rr, [], irNow rr, S.empty) arms
    let now = applyEff su (unionEff residual clauseNow)
    unifyEff resumeEff now
    st <- get
    let outside = out: [TVar v | sch <- M.elems env, v <- S.toList (ftvS' sch)]
    when (any (rigidEscapes rigidNames . freezeType st . apply su) outside) $
        reject "operation type variable escape its clause"
    return $ InferRes su (map (applyWanted su) ws) (apply su out)
       (Handle (irExpr rb) n (map (apply su) actualTs) ret (irExpr rr) arms') (resolveEff st now)

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
                        (map fst (nameGroups (groupWanted (filter deferWanted (irWanted (applyInfer su r))))))
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

widenAnswer :: MonadState Counter m => T' -> m T'
widenAnswer = \case
    TFunc a e b -> freshEff >>= \ex -> TFunc a (unionEff e ex) <$> widenAnswer b
    t -> return t

closeBinding :: Checker m => TEnv -> T' -> [Wanted] -> E' -> Eff -> m (S', E')
closeBinding = closeBindingWithOpen S.empty

closeBindingWithOpen ::  Checker m => Set HoleId -> TEnv -> T' -> [Wanted] -> E' -> Eff -> m (S', E')
closeBindingWithOpen open outer ty0 wanted0 body now = do
    initial <- get
    let provisional = freezeType initial ty0
        anchored = relationClosure initial $
            inputEffectVars provisional `S.union` ftvTEnv outer
        lvs = relationClosure initial $
            typeEffectVars provisional `S.union` effVars (resolveEff initial now)
    forM_ (S.toList (lvs `S.difference` anchored)) $ \v ->
        when (M.notMember v (effDefs initial)
            && S.null (effVars (resolveEff initial (Eff S.empty (S.singleton v))) `S.intersection` anchored)) $
            modify (\c -> c { effDefs = M.insert v emptyEff (effDefs c) })
    validateEffects
    st <- get
    let ty = freezeType st ty0
        callNow = resolveEff st now
        seedVars = S.unions [ftv ty, ftvEff callNow, S.unions (map (ftv . wtdType) wanted0)]
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

    let grouped = nameGroups (groupWanted outwardWs)
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

    body' <- case fillHoles (M.union solved (M.fromList [(h, ImpHole h) | h <- S.toList open])) body of
        Left h  -> throwError $ case [w | w <- concreteWs, wtdHole w == h] of
            w: _ -> Located (wtdSpan w) (EscapedImpHole h)
            [] -> Located Nothing (EscapedImpHole h)
        Right e -> return e

    let bodyB = foldr (\(n, c) b -> App (Lambda n b) c) body' captured
        aliasOne (_, pri, aliases, _) b = foldr (\x acc -> Let x (Var pri) acc) b aliases
        body'' = foldr (Lambda . (\(_, n, _, _) -> n)) (foldr aliasOne bodyB hidden) hidden
        relations = relevantRelations st genVars
        needed = S.unions [ftv ty, ftvPreds predicates, ftvEff callNow,
            S.unions [ftvEff a `S.union` ftvEff b | (a, b) <- relations]] `S.difference` ftvTEnv outer
        scheme = (Forall needed predicates ty)
            { sDictCall = if null hidden then emptyEff else callNow
            , sEffCs    = relations
            , sInitEff  = if null hidden then callNow else emptyEff
            }
    return (scheme, body'')

inferDeclBodyPrepared :: Checker m => TEnv -> IEnv -> [Param] -> E' -> m InferRes
inferDeclBodyPrepared env ienv params body = go env params
    where
        go current = \case
            [] -> inferExpr current ienv body

            (ExplicitParam n mty : rest) -> do
                a <- fresh
                -- 有标注 `(n :: T)`：先把参数类型变量钉到 T，再推断 body，
                -- 于是 body 里的字段访问看到的是具体类型（走单态解析），
                -- 整个 def 也就不会泛化成「任何带该字段的类型」。
                u0 <- case mty of
                    Nothing -> return M.empty
                    Just ty -> do
                        denv <- ask
                        checkTypeCons denv ty
                        unify (a, ty)
                r <- go (applyTEnv u0 (M.insert n (Forall S.empty [] a) current)) rest
                let r' = applyInfer u0 r
                return r'
                    { irType = TFunc (apply (irSub r') a) (irNow r') (irType r')
                    , irExpr = Lambda n (irExpr r')
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
                        , wtdSearch = Just n
                        } : irWanted r
                    }

rigidEscapes :: Set Text -> T' -> Bool
rigidEscapes names = go
    where
        go = \case
            TCon n ts -> S.member n names || any go ts
            TFunc a e b -> go a || go b || any (any (any go)) (M.elems (effParams e))
            TList t -> go t
            TTuple ts -> any go ts
            _ -> False
