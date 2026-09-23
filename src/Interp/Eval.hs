{-# LANGUAGE LambdaCase, FlexibleContexts, TupleSections #-}

module Interp.Eval where
import Interp.Types
import Data.Map ((!?))
import qualified Data.Map as M
import Control.Monad.Reader (MonadReader, ask, local, asks)
import Control.Monad.Error.Class (MonadError (throwError, catchError))
import Control.Monad.State (MonadState (get, put), modify)
import Control.Monad (when, foldM, forM)
import Data.Graph (stronglyConnComp, SCC (AcyclicSCC, CyclicSCC))
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (isJust)

evalwDepth :: (MonadState Depth m, MonadReader Env m, MonadError (Located EvalError) m) => E' -> m V'
evalwDepth expr = do
    d <- get
    when (d > maxDepth) $
        throwError $ Located Nothing (RecursionLimited d)
    modify $ \(Depth n) -> Depth (n + 1)
    res <- eval expr
    put d
    return res

eval :: (MonadReader Env m, MonadError (Located EvalError) m) => E' -> m V'
eval = \case
    ILit i -> return $ VInt i

    BLit b -> return $ VBool b

    Var v -> do
        env <- ask
        case env !? v of
            Nothing -> throwError (Located Nothing $ UnboundVariable v)
            Just v' -> return v'

    BOpr bopr e1 e2 -> do
        v1 <- eval e1
        v2 <- eval e2
        case (v1, v2) of
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
        case v1 of
            VClosure t ec env -> do
                v2 <- eval e2
                local (const (M.insert t v2 env)) (eval ec)
            _ -> throwError (Located Nothing $ IsNotFunction v1)

evalProgram :: (MonadState Depth m, MonadReader Env m, MonadError (Located EvalError) m) => Program -> m (Env, M.Map Int (Either (Located EvalError) V'))
evalProgram prs = ask >>= \env -> do
    env1 <- foldM step env (stronglyConnComp (deps prs))
    vals <- forM [(i, e) | (i, StmtExpr e) <- zip [0..] prs] $ \(i, e) -> (i, ) <$> tryEnv (local (const env1) (evalwDepth e))
    return (env1, M.fromList vals)
    where
        step :: (MonadState Depth m, MonadReader Env m, MonadError (Located EvalError) m) => Env -> SCC Decl -> m Env
        step env scc = case flatten scc of
            [Def n [] b] | not (isLambda b) -> do
                v <- local (const env) (evalwDepth b)
                return $ M.insert n v env
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

emptyMap :: M.Map a b
emptyMap = M.empty

addSpan :: Span -> Located e -> Located e
addSpan sp (Located Nothing err) = Located (Just sp) err
addSpan _ l@(Located (Just _) _) = l

stripAt :: E' -> E'
stripAt = \case
    At _ e -> e
    e      -> e

maxDepth :: Depth
maxDepth = Depth 1000

freeVars :: E' -> Set Text
freeVars = \case
    Var v -> S.singleton v
    ILit _ -> S.empty
    BLit _ -> S.empty
    Lambda x e -> S.delete x (freeVars e)
    Let t e1 e2 -> freeVars e1 `S.union` S.delete t (freeVars e2)
    If b e1 e2 -> S.unions (map freeVars [b, e1, e2])
    BOpr _ e1 e2 -> freeVars e1 `S.union` freeVars e2
    App f a -> freeVars f `S.union` freeVars a
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

relabel :: [(Int, Statement)] -> M.Map Int a -> M.Map Int a
relabel lprs m = M.fromList [(l, v) | (k, v) <- M.toList m, Just l <- [M.lookup k idx]]
    where idx = M.fromList (zip [0 ..] (map fst lprs))