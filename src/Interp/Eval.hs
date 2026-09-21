{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Interp.Eval where
import Interp.Types
import Data.Map ((!?))
import qualified Data.Map as M
import Control.Monad.Reader (MonadReader, ask, local, asks)
import Control.Monad.Error.Class (MonadError (throwError))

eval :: (MonadReader Env m, MonadError EvalError m) => E' -> m V'
eval = \case
    Lit i -> return $ VInt i

    Var v -> do
        env <- ask
        case env !? v of
            Nothing -> throwError(UnboundVariable v)
            Just v' -> return v'

    ArithOpr opr e1 e2 -> do
        v1 <- eval e1
        v2 <- eval e2
        case (v1, v2) of
            (VInt vi1, VInt vi2) -> case opr of
                OpDiv | vi2 == 0 -> throwError (DividedByZero vi2)
                OpPow | vi2 < 0  -> throwError (NegativeExponent vi2)
                _                -> return $ VInt $ calArithOpr opr vi1 vi2
            (VInt _, _) -> throwError (ArithArgIsNotNum opr v2)
            _           -> throwError (ArithArgIsNotNum opr v1)

    Let t e1 e2 -> eval e1 >>= \v -> local (M.insert t v) (eval e2)

    Lambda t expr -> asks $ VClosure t expr

    App e1 e2 -> do
        v1 <- eval e1
        case v1 of
            VClosure t ec env -> do
                v2 <- eval e2
                local (const (M.insert t v2 env)) (eval ec)
            _ -> throwError (IsNotFunction v1)

calArithOpr :: (Integral a, Num a) => LitOpr -> (a -> a -> a)
calArithOpr = \case
    OpAdd -> (+)
    OpSub -> (-)
    OpMul -> (*)
    OpDiv -> div
    OpPow -> (^)

emptyMap :: M.Map a b
emptyMap = M.empty