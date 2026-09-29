{-# LANGUAGE OverloadedStrings #-}
module Interp.Builtin where

import Interp.Types

import qualified Data.Map as M
import qualified Data.Set as S

builtinEnv :: Env 
builtinEnv = M.fromList
    [ ("#cons", VPrim "#cons" 2 [])
    , ("cons", VPrim "#cons" 2 [])
    , ("++", VPrim "++" 2 [])
    , ("uncons", VPrim "uncons" 1 [])
    , ("fromCode", VPrim "fromCode" 1 [])
    ]

builtinTEnv :: TEnv
builtinTEnv = M.fromList
    [ ("#cons", consTy)
    , ("cons", consTy)
    , ("++", strAppTy)
    , ("uncons", unconsTy)
    , ("fromCode", fromCodeTy)
    ]

consTy :: S'
consTy = Forall (S.singleton 0) (TFunc (TVar 0) (TFunc (TList (TVar 0)) (TList (TVar 0))))

-- Strings are a primitive type with exactly three primitives: `++`, and the
-- pair that makes a string walkable -- the head code point plus the rest,
-- and a code point back into a one character string. Everything else
-- (`length`, `words`, `toUpper`, ...) is an ordinary `def` in prelude.txt,
-- the same way the list library is built on `#cons` and `match`.
--
-- Characters are `Int` code points, not a type of their own: `Int` already
-- has `<`, so `c1 < c2` works for free.
strAppTy :: S'
strAppTy = Forall S.empty (TFunc TString (TFunc TString TString))

unconsTy :: S'
unconsTy = Forall S.empty (TFunc TString (TCon "Maybe" [TTuple [TInt, TString]]))

fromCodeTy :: S'
fromCodeTy = Forall S.empty (TFunc TInt TString)

initSession :: Session
initSession = Session 
    { sEnv     = builtinEnv
    , sTEnv    = builtinTEnv
    , sNext    = 0
    , sBatch   = ""
    , sDEnv    = emptyDEnv
    , sAliases = M.empty
    , sSyns    = M.empty
    }

emptyDEnv :: DEnv
emptyDEnv = DEnv M.empty M.empty