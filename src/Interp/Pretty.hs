{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Interp.Pretty where

import Interp.Types
import Data.Text (Text, pack)
import qualified Data.Text as T

prettyArithOp :: LitOpr -> Text
prettyArithOp = pack . drop 2 . show

prettyE' :: E' -> Text
prettyE' = \case
    Lit n -> pack (show n)
    Var t -> t
    ArithOpr op e1 e2 -> "(" <> T.unwords [prettyArithOp op, prettyE' e1, prettyE' e2] <> ")"
    Let t e1 e2 -> T.unwords ["(Let", t, "=", prettyE' e1, "in", prettyE' e2] <> ")"
    Lambda t e -> T.concat ["(\\", t, " -> ", prettyE' e, ")"]
    App e1 e2 -> "(App " <> T.unwords [prettyE' e1, prettyE' e2] <> ")"

prettyV' :: V' -> Text
prettyV' = \case
    VInt n -> pack $ show n
    VClosure t _ _ -> T.unwords ["<closure", t, "->", "...>"]

prettyEvalError :: EvalError -> Text
prettyEvalError = \case
    UnboundVariable t -> t <> " is an unbound variable."
    ArithArgIsNotNum op v -> T.unwords ["Argument", prettyV' v, "of", prettyArithOp op, "expects a number."]
    IsNotFunction v -> prettyV' v <> " is not a function."
    DividedByZero i -> "Divided by zero at " <> pack (show i) <> "."
    NegativeExponent i -> "Negative exponent at " <> pack (show i) <> "."

prettyT' :: T' -> Text
prettyT' = \case
    TInt -> "Int"
    TVar i -> "a" <> pack (show i)
    TFunc t1 t2 -> "(" <> T.unwords [prettyT' t1, "->", prettyT' t2] <> ")"

prettyTypeError :: TypeError -> Text
prettyTypeError = \case
    UnboundVar t -> T.unwords [t, "is an unbound variable."]
    TypeMismatch t -> T.unwords ["Type mismath,", t, "."]
    OccurCheck -> "Ocuur type check."