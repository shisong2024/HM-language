{-# LANGUAGE OverloadedStrings #-}
module Interp.Types where

import Data.Map (Map)
import Text.Megaparsec (Parsec)
import Data.Text (Text)
import Data.Void (Void)
import Data.Set (Set)

data E'
    = Lit Int
    | Var Text
    | Let Text E' E'
    | ArithOpr LitOpr E' E'
    | Lambda Text E'
    | App E' E'
    deriving (Show, Eq)

data LitOpr = OpAdd | OpMul | OpSub | OpDiv | OpPow deriving (Show, Eq)
data Assoc  = AssocL | AssocR deriving (Show, Eq)

opTable :: [(Text, LitOpr)]
opTable = [("+", OpAdd), ("-", OpSub), ("*", OpMul), ("/", OpDiv), ("^", OpPow)]

type Lev = Int

data V'
    = VInt Int
    | VClosure Text E' Env
    deriving (Show, Eq)

data EvalError
    = UnboundVariable Text
    | ArithArgIsNotNum LitOpr V'
    | IsNotFunction V'
    | DividedByZero Int
    | NegativeExponent Int
    deriving (Show, Eq)

data T' = TInt | TVar TypeVar | TFunc T' T' deriving (Show, Eq)

type TypeVar = Int
type Counter = Int

data TypeError
    = UnboundVar Text
    | TypeMismatch Text
    | OccurCheck
    deriving (Show, Eq)

type Env  = Map Text V'
type TEnv = Map Text S'
type TSub = Map TypeVar T'
type Parser a = Parsec Void Text a

data S' = Forall (Set TypeVar) T' deriving (Show, Eq)