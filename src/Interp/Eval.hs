{-# LANGUAGE LambdaCase, FlexibleContexts, TupleSections, OverloadedStrings #-}

module Interp.Eval where
import Interp.Types
import Data.Map ((!?))
import qualified Data.Map as M
import Control.Monad.Reader (MonadReader, ask, local, asks)
import Control.Monad.Error.Class (MonadError (throwError, catchError))
import Control.Monad.State (MonadState (get, put), modify)
import Control.Monad (when, foldM, forM)
import Data.Graph (stronglyConnComp, SCC (AcyclicSCC, CyclicSCC))
import Data.Text (Text, unpack)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (isJust)

evalwDepth :: (MonadState Depth m, MonadReader Env m, MonadError (Located EvalError) m) => E' -> m V'
evalwDepth expr = do
    d <- get
    when (d > maxDepth) $
        throwError $ Located Nothing (RecursionLimited d)
    modify $ \(Depth n) -> Depth (n + 1)
    res <- eval expr `catchError` \e -> put d >> throwError e
    put d >> return res

eval :: (MonadState Depth m, MonadReader Env m, MonadError (Located EvalError) m) => E' -> m V'
eval = \case
    ILit i -> return $ VInt i

    BLit b -> return $ VBool b

    Var v -> do
        env <- ask
        case env !? v of
            Nothing -> throwError (Located Nothing $ UnboundVariable v)
            Just v' -> return v'

    ListLit es -> VList <$> mapM eval es

    TupleLit es -> VTuple <$> mapM eval es

    BOpr bopr e1 e2 -> do
        v1 <- eval e1
        v2 <- eval e2
        case bopr of
            OCmp OpEq -> eqOp v1 v2 True
            OCmp OpNe -> eqOp v1 v2 False
            _ -> case (v1, v2) of
                (VInt vi1, VInt vi2) -> case bopr of
                    OArith OpDiv | vi2 == 0 -> throwError (Located Nothing $ DividedByZero vi2)
                    OArith OpPow | vi2 < 0  -> throwError (Located Nothing $ NegativeExponent vi2)
                    _  -> return $ case bopr of
                        OArith opr -> VInt $ calArithOpr opr vi1 vi2
                        OCmp opr   -> VBool $ calCmpOpr opr vi1 vi2
                (VInt _, _) -> throwError (Located Nothing $ OprArgIsNotNum bopr v2)
                _           -> throwError (Located Nothing $ OprArgIsNotNum bopr v1)

    At sp e -> catchError (eval e) (throwError . addSpan sp)

    Let t e1 e2 -> do
        env <- ask
        case stripAt e1 of
            Lambda x eb ->
                let recEnv = M.insert t (VClosure x eb recEnv) env in
                local (const recEnv) (eval e2)
            _ -> eval e1 >>= \v -> local (const (M.insert t v env)) (eval e2)

    If eb e1 e2 -> do
        mb <- eval eb
        case mb of
            VBool b -> eval (if b then e1 else e2)
            _       -> throwError (Located Nothing $ IfNeedsBool mb)

    Lambda t expr -> asks $ VClosure t expr

    App e1 e2 -> do
        v1 <- eval e1
        v2 <- eval e2
        case v1 of
            VClosure t ec env -> local (const (M.insert t v2 env)) (evalwDepth ec)
            VPrim n ar args -> do
                let args' = args ++ [v2]
                if length args' < ar then return $ VPrim n ar args'
                else applyPrim n args'
            VCtor n ar args -> do
                let args' = args ++ [v2]
                if length args' <= ar then return $ VCtor n ar args'
                else throwError $ Located Nothing $ IsNotFunction v1
            _ -> throwError (Located Nothing $ IsNotFunction v1)
    
    Match e pes -> eval e >>= \v -> matchArms v pes

    AnnT e _ -> eval e

evalProgram :: (MonadState Depth m, MonadReader Env m, MonadError (Located EvalError) m) => Program -> m (Env, M.Map Int (Either (Located EvalError) V'))
evalProgram prs = ask >>= \env -> do
    let env0 = M.union (ctorValues (dataDeclsOf prs)) env
    env1 <- foldM step env0 (stronglyConnComp (deps prs))
    vals <- forM [(i, e) | (i, StmtExpr e) <- zip [0..] prs] $ \(i, e) -> (i, ) <$> tryEnv (local (const env1) (evalwDepth e))
    return (env1, M.fromList vals)
    where
        step :: (MonadState Depth m, MonadReader Env m, MonadError (Located EvalError) m) => Env -> SCC Decl -> m Env
        step env scc = case scc of
            AcyclicSCC (Def n [] b) | not (isLambda b) -> do
                v <- local (const env) (evalwDepth b)
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
maxDepth = Depth 10000

freeVars :: E' -> Set Text
freeVars = \case
    Var v -> S.singleton v
    ILit _ -> S.empty
    BLit _ -> S.empty
    ListLit vs -> S.unions (map freeVars vs)
    TupleLit vs -> S.unions (map freeVars vs)
    Lambda x e -> S.delete x (freeVars e)
    Let t e1 e2 -> freeVars e1 `S.union` S.delete t (freeVars e2)
    If b e1 e2 -> S.unions (map freeVars [b, e1, e2])
    BOpr _ e1 e2 -> freeVars e1 `S.union` freeVars e2
    App f a -> freeVars f `S.union` freeVars a
    Match e pes -> S.unions (freeVars e: [freeVars b `S.difference` patVars p | (p, b) <- pes])
    AnnT e _ -> freeVars e
    At _ e -> freeVars e

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

    (PNil, VList []) -> Just []
    (PCons ph pt, VList (x: xs)) -> (++) <$> matchP (ph, x) <*> matchP (pt, VList xs)

    (PTuple ps, VTuple vs) | length ps == length vs -> 
        concat <$> mapM matchP (zip ps vs)

    (PCtor n ps, VCtor m _ qs) | n == m && length ps == length qs -> 
        concat <$> mapM matchP (zip ps qs)
    
    _ -> Nothing

applyPrim :: MonadError (Located EvalError) m => Text -> [V'] -> m V'
applyPrim "#cons" [h, t] = case t of
    VList xs -> return $ VList (h: xs)
    _ -> throwError $ Located Nothing $ ConsNeedsList t
applyPrim n _ = error $ unpack ("unknown primitive: " <> n)

matchArms :: (MonadError (Located EvalError) m, MonadReader Env m,  MonadState Depth m) => V' -> [(P', E')] -> m V'
matchArms v = \case
    [] -> throwError $ Located Nothing $ NonExhaustiveMatch v
    (p, b): rest -> case matchP (p, v) of
        Nothing   -> matchArms v rest
        Just bnds -> local (M.union (M.fromList bnds)) (eval b)

eqV :: (V', V') -> Maybe Bool
eqV = \case
    (VInt a, VInt b) -> Just (a == b)
    (VBool a, VBool b) -> Just (a == b)
    (VList a, VList b) -> if length a == length b then and <$> mapM eqV (zip a b) else Just False
    (VTuple a, VTuple b) -> if length a == length b then and <$> mapM eqV (zip a b) else Just False
    (VCtor n _ a, VCtor m _ b) -> 
        if n == m && length a == length b then and <$> mapM eqV (zip a b) else Just False
    _ -> Nothing

eqOp :: MonadError (Located EvalError) m => V' -> V' -> Bool -> m V'
eqOp v1 v2 w = case eqV (v1, v2) of
    Just b -> return $ VBool (if w then b else not b)
    Nothing -> throwError $ Located Nothing $ OprArgIsNotComparable v1 v2

dataDeclsOf :: Program -> [DataDecl]
dataDeclsOf prs = [d | StmtData d <- prs]