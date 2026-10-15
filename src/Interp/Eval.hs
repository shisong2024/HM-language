{-# LANGUAGE LambdaCase, FlexibleContexts, TupleSections, OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Interp.Eval where
import Interp.Types
import Interp.Parser (dataDeclsOf)
import Interp.Computation

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Control.Monad (when, foldM, forM)
import Control.Monad.Reader (MonadReader, ask, local)
import Control.Monad.Error.Class (MonadError (throwError, catchError))
import Control.Monad.State (MonadState, modify, gets)

import Data.Map ((!?))
import Data.Graph (stronglyConnComp, SCC (AcyclicSCC, CyclicSCC))
import Data.Char (chr, ord)
import Data.Text (Text)
import Data.Set (Set)
import Data.Maybe (isJust)

type Eval m = (MonadState Runtime m, MonadReader Env m, MonadError (Located EvalError) m)

evalwDepth :: Eval m => E' -> m V'
evalwDepth expr = do
    d <- gets rtDepth
    when (d > maxDepth) $
        throwError $ Located Nothing (RecursionLimited d)
    let restoreDepth = modify (\rt -> rt {rtDepth = d})
    modify $ \rt -> rt {rtDepth = d + 1}
    res <- eval expr `catchError` \e -> restoreDepth >> throwError e
    restoreDepth
    return res

eval :: Eval m => E' -> m V'
eval e = ask >>= \env -> gets rtDepth >>= \d -> runC (evaluate env d e)

runC :: Eval m => Computation -> m V'
runC = runCWith defaultHost

defaultHost :: Eval m => Text -> Text -> V' -> m V'
defaultHost t o v = if t == "Console" then applyPrim o [v] else throwError (Located Nothing (UnhandledEff t o))

runCWith :: Eval m => (Text -> Text -> V' -> m V') -> Computation -> m V'
runCWith host = loop
    where
        loop = \case
            Done v -> return v
            Failed e -> throwError e
            Request n o v sp (Continuation k) -> atSource sp (host n o v) >>= loop . k
            Primitive n vs env sp (Continuation k) -> atSource sp (local (const env) (applyPrim n vs)) >>= loop . k
            FreshResume k -> resumeNext >>= loop . k
            UseResume i sp c -> do
                used <- gets rtResumeUsed
                when (S.member i used) $ throwError $ Located sp $ ResumeAlreadyUsed i
                modify (\rt -> rt { rtResumeUsed = S.insert i (rtResumeUsed rt) })
                loop c
        
        atSource m action = case m of
            Nothing -> action
            Just sp -> action `catchError` (throwError . addSpan sp)
                
resumeNext :: MonadState Runtime m => m Int
resumeNext = do
    i <- gets rtResumeNext
    modify (\rt -> rt { rtResumeNext = i + 1 })
    return i

evalWith :: Eval m => (Text -> Text -> V' -> m V') -> E' -> m V'
evalWith host e = ask >>= \env -> gets rtDepth >>= \depth -> runCWith host (evaluate env depth e)

evaluate :: Env -> Int -> E' -> Computation
evaluate env d e = 
    if d > maxDepth then Failed (Located Nothing (RecursionLimited d)) 
    else case e of
        ILit i -> Done $ VInt i
        BLit b -> Done $ VBool b
        SLit s -> Done $ VStr s
        Var v -> maybe (Failed $ Located Nothing $ UnboundVariable v) Done (M.lookup v env)
        ListLit es -> values es (Done . VList)
        TupleLit es -> values es (Done . VTuple)
        BOpr bopr e1 e2 -> bindC (go e1) $ \va -> bindC (go e2) $ \vb -> arithmetic bopr va vb
        At sp ex -> mapFailure sp (go ex)
        Let t e1 e2 -> case stripAt e1 of
            Lambda x eb ->
                let recEnv = M.insert t (VClosure x eb recEnv) env in evaluate recEnv d e2
            _ -> bindC (go e1) (\v -> evaluate (M.insert t v env) d e2)
        StrictLet t e1 e2 -> bindC (go e1) (\v -> evaluate (M.insert t v env) d e2)
        Handle b n _ r rb arms -> handleC evaluate env d n r rb arms (go b)
        DictCall {} -> Failed $ Located Nothing UnelaboratedDictCall
        If c y n -> bindC (go c) $ \case
            VBool b -> go (if b then y else n)
            v       -> Failed $ Located Nothing $ IfNeedsBool v
        Lambda t expr -> Done $ VClosure t expr env
        App f a -> bindC (go f) $ \vf -> bindC (go a) $ \va -> applyValue vf va
        Match s arms -> bindC (go s) (`arm` arms)
        AnnT a _ -> go a
        ImpHole h -> Failed (Located Nothing (UnboundVariable
            ("internal implicit hole " <> T.pack (show h))))
    where
        go = evaluate env d
        values l k = case l of
            [] -> k []
            x: xs -> bindC (go x) (\v -> values xs (k . (v: )))
        arm v = \case
            [] -> Failed $ Located Nothing $ NonExhaustiveMatch v
            (p, b): rest -> case matchP (p, v) of
                Nothing -> arm v rest
                Just bs -> evaluate (M.union (M.fromList bs) env) d b
        applyValue f v = case f of
            VClosure n b cap -> evaluate (M.insert n v cap) (d + 1) b
            VOperation n o -> Request n o v Nothing (Continuation Done)
            VResume i (Continuation k) -> UseResume i Nothing (k v)
            VPrim n ar args -> let args' = args ++ [v] in
                if length args' < ar then Done $ VPrim n ar args'
                else if n == "putStr" || n == "putStrLn" then Request "Console" n v Nothing (Continuation Done)
                else Primitive n args' env Nothing (Continuation Done)
            VCtor n ar args | length args < ar -> Done $ VCtor n ar (args ++ [v])
            _ -> Failed (Located Nothing $ IsNotFunction f)
        arithmetic bopr e1 e2 = case bopr of
            OCmp c | c == OpEq || c == OpNe -> case eqV (e1, e2) of
                Just x -> Done $ VBool (if c == OpEq then x else not x)
                Nothing -> Failed $ Located Nothing $ OprArgIsNotComparable e1 e2
            _ -> case (e1, e2) of
                (VInt vi1, VInt vi2) -> case bopr of
                    OArith OpDiv | vi2 == 0 -> Failed (Located Nothing $ DividedByZero vi2)
                    OArith OpPow | vi2 < 0  -> Failed (Located Nothing $ NegativeExponent vi2)
                    OArith opr -> Done $ VInt $ calArithOpr opr vi1 vi2
                    OCmp opr   -> Done $ VBool $ calCmpOpr opr vi1 vi2
                (VInt _, _) -> Failed (Located Nothing $ OprArgIsNotNum bopr e2)
                _ -> Failed (Located Nothing $ OprArgIsNotNum bopr e1)
 
evalProgram :: Eval m => Program -> m (Env, M.Map Int (Either (Located EvalError) V'))
evalProgram = evalProgramSkipping S.empty

evalCheckedProgram :: Eval m => CProgram -> m (Env, M.Map Int (Either (Located EvalError) V'))
evalCheckedProgram checked = evalProgramSkipping skipped (cProgram checked)
  where
    skipped = S.fromList [i | (i, TyExpr (Left _)) <- M.toList (cTypes checked)]

evalCheckedProgramWith :: Eval m => (Text -> Text -> V' -> m V') -> CProgram -> m (Env, M.Map Int (Either (Located EvalError) V'))
evalCheckedProgramWith host checked = evalProgramSkippingWith host 
    (S.fromList [i | (i, TyExpr (Left _)) <- M.toList (cTypes checked)]) (cProgram checked)

evalProgramSkipping :: Eval m => Set Int -> Program -> m (Env, M.Map Int (Either (Located EvalError) V'))
evalProgramSkipping = evalProgramSkippingWith defaultHost

evalProgramSkippingWith :: Eval m => (Text -> Text -> V' -> m V') -> Set Int -> [Statement] -> m (Env, M.Map Int (Either (Located EvalError) V'))
evalProgramSkippingWith host skipped prs = ask >>= \env -> do
    let operations = M.fromList [(o, VOperation (edName d) o) | StmtOperations d <- prs, (o, _) <- edOperations d]
        env0 = M.unions [operations, ctorValues (dataDeclsOf prs), env]
    env1 <- foldM step env0 (stronglyConnComp (deps prs))
    vals <- forM [(i, e) | (i, StmtExpr e) <- zip [0..] prs, not (S.member i skipped)] $ \(i, e) -> (i, ) <$> tryEnv (local (const env1) (evalWith host e))
    return (env1, M.fromList vals)
    where
        step env scc = case scc of
            AcyclicSCC (Def n [] b) | not (isLambda b) -> do
                v <- local (const env) (evalWith host b)
                return $ M.insert n v env
            _ -> case flatten scc of
                ds | not $ all canKnot ds -> 
                    throwError (Located Nothing (RecursiveVarDef $ (\(Def n _ _) -> n) $ head ds))
                ds -> do
                    let recEnv = M.union grp env
                        grp = M.fromList [(n, clo d recEnv) | d@(Def n _ _) <- ds]
                    return $ M.union grp env

        isLambda :: E' -> Bool
        isLambda e = case stripAt e of Lambda _ _ -> True; _ -> False

        defParts :: Decl -> Maybe (Text, E')
        defParts (Def _ (_: _) b) = case b of Lambda v r -> Just (v, r); _ -> Nothing
        defParts (Def _ []     b) = case stripAt b of Lambda v r -> Just (v, r); _ -> Nothing

        canKnot :: Decl -> Bool
        canKnot = isJust . defParts

        clo :: Decl -> Env -> V'
        clo d cap = case defParts d of
            Just (v, b) -> VClosure v b cap
            Nothing     -> error "unexpected."

        ctorValues :: [DataDecl] -> Env
        ctorValues ds = M.fromList [(c, VCtor c (length args) []) | d <- ds, (c, args) <- dCtors d]

calArithOpr :: (Integral a, Num a) => LitOpr -> (a -> a -> a)
calArithOpr = \case
    OpAdd -> (+)
    OpSub -> (-)
    OpMul -> (*)
    OpDiv -> div
    OpPow -> (^)

calCmpOpr :: Integral a => CmpOpr -> (a -> a -> Bool)
calCmpOpr = \case
    OpEq -> (==)
    OpNe -> (/=)
    OpLe -> (<=)
    OpGe -> (>=)
    OpLt -> (<)
    OpGt -> (>)

addSpan :: Span -> Located e -> Located e
addSpan sp (Located Nothing err) = Located (Just sp) err
addSpan _ l@(Located (Just _) _) = l

stripAt :: E' -> E'
stripAt = \case
    At _ e -> stripAt e
    AnnT e _ -> stripAt e
    e      -> e

maxDepth :: Depth
maxDepth = 10000

freeVars :: E' -> Set Text
freeVars = \case
    Var v -> S.singleton v
    ILit _ -> S.empty
    BLit _ -> S.empty
    SLit _ -> S.empty
    ListLit vs -> S.unions (map freeVars vs)
    TupleLit vs -> S.unions (map freeVars vs)
    Lambda x e -> S.delete x (freeVars e)
    Let t e1 e2 -> freeVars e1 `S.union` S.delete t (freeVars e2)
    StrictLet t e1 e2 -> freeVars e1 `S.union` S.delete t (freeVars e2)
    Handle b _ _ r rb os -> S.unions (freeVars b: S.delete r (freeVars rb): 
        [freeVars e `S.difference` S.fromList [x, k] | (_, x, k, e) <- os])
    DictCall h as ds -> S.unions (map freeVars (h: as <> map daExpr ds))
    If b e1 e2 -> S.unions (map freeVars [b, e1, e2])
    BOpr _ e1 e2 -> freeVars e1 `S.union` freeVars e2
    App f a -> freeVars f `S.union` freeVars a
    Match e pes -> S.unions (freeVars e: [freeVars b `S.difference` patVars p | (p, b) <- pes])
    AnnT e _ -> freeVars e
    At _ e -> freeVars e
    ImpHole _ -> S.empty

deps :: Program -> [(Decl, Text, [Text])]
deps prs = 
    [ (d, n, filter (`elem` [na | StmtDef (Def na _ _) <- prs]) (S.toList (freeVars b)))
    | (StmtDef d@(Def n _ b)) <- prs
    ]

flatten :: SCC Decl -> [Decl]
flatten = \case
    AcyclicSCC d -> [d]
    CyclicSCC ds -> ds

tryEnv :: MonadError e m => m a -> m (Either e a)
tryEnv act = (Right <$> act) `catchError` (return . Left)

patVars :: P' -> Set Text
patVars = S.fromList . patVarsList

patVarsList :: P' -> [Text]
patVarsList = \case
    PVar x -> [x]
    PCons p q -> patVarsList p ++ patVarsList q
    PTuple ps -> concatMap patVarsList ps
    PCtor _ ps -> concatMap patVarsList ps
    _ -> []

matchP :: (P', V') -> Maybe [(Text, V')]
matchP = \case
    (PWild, _) -> Just []
    (PVar x, v) -> Just [(x, v)]

    (PInt n, VInt m) | n == m -> Just []
    (PBool n, VBool m) | n == m -> Just []
    (PStr a, VStr b) | a == b -> Just []

    (PNil, VList []) -> Just []
    (PCons ph pt, VList (x: xs)) -> (++) <$> matchP (ph, x) <*> matchP (pt, VList xs)

    (PTuple ps, VTuple vs) | length ps == length vs -> 
        concat <$> mapM matchP (zip ps vs)

    (PCtor n ps, VCtor m _ qs) | n == m && length ps == length qs -> 
        concat <$> mapM matchP (zip ps qs)
    
    _ -> Nothing

applyPrim :: Eval m => Text -> [V'] -> m V'
applyPrim "putStr" [VStr text] = recordOutput (WriteText text)
applyPrim "putStrLn" [VStr text] = recordOutput (WriteLine text)
applyPrim "#cons" [h, t] = case t of
    VList xs -> return $ VList (h: xs)
    _ -> throwError $ Located Nothing $ ConsNeedsList t
applyPrim "++" [VStr a, VStr b] = return $ VStr (a <> b)
applyPrim "fromCode" [VInt i]
    | i < 0 || i > 0xFFFF = throwError $ Located Nothing $ NotACodepoint i
    | otherwise = return $ VStr (T.singleton (chr (fromInteger i)))
applyPrim "uncons" [VStr s] = case T.uncons s of
    Nothing -> ctor "None" []
    Just (c, rest) -> ctor "Just" [VTuple [VInt (fromIntegral (ord c)), VStr rest]]
    where
        ctor n args = do
            env <- ask
            case env !? n of
                Just (VCtor _ ar _) -> return $ VCtor n ar args
                _ -> throwError $ Located Nothing $ UnboundVariable n
applyPrim "intCompare" [VInt a, VInt b] = return $ VInt $ case compare a b of
    LT -> -1; EQ -> 0; GT -> 1
applyPrim "$fieldErr" [VStr f, v] = throwError $ Located Nothing $ NoSuchField f v
applyPrim n args = throwError $ Located Nothing $ PrimArgMismatch n args

recordOutput :: Eval m => OutputEvent -> m V'
recordOutput event = do
    env <- ask
    unit <- case env !? "Unit" of
        Just value@(VCtor "Unit" 0 []) -> return value
        _ -> throwError $ Located Nothing $ UnboundVariable "Unit"
    modify $ \rt -> rt {rtLog = rtLog rt Seq.|> event}
    return unit

matchArms :: (MonadError (Located EvalError) m, MonadReader Env m,  MonadState Runtime m) => V' -> [(P', E')] -> m V'
matchArms v = \case
    [] -> throwError $ Located Nothing $ NonExhaustiveMatch v
    (p, b): rest -> case matchP (p, v) of
        Nothing   -> matchArms v rest
        Just bnds -> local (M.union (M.fromList bnds)) (eval b)

eqV :: (V', V') -> Maybe Bool
eqV = \case
    (VInt a, VInt b) -> Just (a == b)
    (VBool a, VBool b) -> Just (a == b)
    (VStr a, VStr b) -> Just (a == b)
    (VList a, VList b) -> if length a == length b then and <$> mapM eqV (zip a b) else Just False
    (VTuple a, VTuple b) -> if length a == length b then and <$> mapM eqV (zip a b) else Just False
    (VCtor n _ a, VCtor m _ b) -> 
        if n == m && length a == length b then and <$> mapM eqV (zip a b) else Just False
    _ -> Nothing

eqOp :: MonadError (Located EvalError) m => V' -> V' -> Bool -> m V'
eqOp v1 v2 w = case eqV (v1, v2) of
    Just b -> return $ VBool (if w then b else not b)
    Nothing -> throwError $ Located Nothing $ OprArgIsNotComparable v1 v2

paramName :: Param -> Text
paramName = \case
    ExplicitParam n -> n
    ImplicitParam n _ -> n

explicitParams :: [Param] -> [Text]
explicitParams ps = [n | ExplicitParam n <- ps]

implicitParams :: [Param] -> [(Text, T')]
implicitParams ps = [(n, t) | ImplicitParam n t <- ps]

declBody :: Decl -> E'
declBody (Def _ ps body) = foldr (Lambda . paramName) body ps

defsOf :: [Statement] -> [Decl]
defsOf prs = [d | StmtDef d <- prs] <> [d | StmtImp d <- prs]