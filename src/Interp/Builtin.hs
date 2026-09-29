{-# LANGUAGE OverloadedStrings #-}
module Interp.Builtin where

import Interp.Types

import qualified Data.Map as M
import qualified Data.Set as S

builtinEnv :: Env 
builtinEnv = M.fromList
    [ ("#cons", VPrim "#cons" 2 [])
    , ("cons", VPrim "#cons" 2 [])
    ]

builtinTEnv :: TEnv
builtinTEnv = M.fromList
    [ ("#cons", consTy)
    , ("cons", consTy)
    ]

consTy :: S'
consTy = Forall (S.singleton 0) (TFunc (TVar 0) (TFunc (TList (TVar 0)) (TList (TVar 0))))

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