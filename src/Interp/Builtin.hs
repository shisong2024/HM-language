{-# LANGUAGE OverloadedStrings #-}
module Interp.Builtin where

import qualified Data.Map as M
import qualified Data.Set as S

import Interp.Types

builtinEnv :: Env 
builtinEnv = M.fromList
    [ ("cons", VPrim "cons" 2 [])
    ]

builtinTEnv :: TEnv
builtinTEnv = M.fromList
    [ ("cons", Forall (S.singleton 0) (TFunc (TVar 0) (TFunc (TList (TVar 0)) (TList (TVar 0)))))
    ]

initSession :: Session
initSession = Session { sEnv = builtinEnv, sTEnv = builtinTEnv, sNext = 0 }