{-# LANGUAGE OverloadedStrings #-}
module Interp.Builtin where

import Interp.Types

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq

builtinEnv :: Env 
builtinEnv = M.fromList
    [ ("#cons", VPrim "#cons" 2 [])
    , ("cons", VPrim "#cons" 2 [])
    , ("++", VPrim "++" 2 [])
    , ("uncons", VPrim "uncons" 1 [])
    , ("fromCode", VPrim "fromCode" 1 [])
    , ("intCompare", VPrim "intCompare" 2 [])
    , ("putStr", VPrim "putStr" 1 [])
    , ("putStrLn", VPrim "putStrLn" 1 [])
    , ("$fieldErr", VPrim "$fieldErr" 2 [])
    ]

builtinTEnv :: TEnv
builtinTEnv = M.fromList
    [ ("#cons", consTy)
    , ("cons", consTy)
    , ("++", strAppTy)
    , ("uncons", unconsTy)
    , ("fromCode", fromCodeTy)
    , ("intCompare", intCompareTy)
    , ("putStr", outputTy)
    , ("putStrLn", outputTy)
    , ("$fieldErr", fieldErrTy)
    ]

outputTy :: S'
outputTy = Forall S.empty [] (TFunc TString consoleEff (TCon "Unit" []))

fieldErrTy :: S'
fieldErrTy = Forall (S.fromList [0, 1]) [] (pureFunc TString (pureFunc (TVar 0) (TVar 1)))

consTy :: S'
consTy = Forall (S.singleton 0) [] (pureFunc (TVar 0) (pureFunc (TList (TVar 0)) (TList (TVar 0))))

strAppTy :: S'
strAppTy = Forall S.empty [] (pureFunc TString (pureFunc TString TString))

unconsTy :: S'
unconsTy = Forall S.empty [] (pureFunc TString (TCon "Maybe" [TTuple [TInt, TString]]))

fromCodeTy :: S'
fromCodeTy = Forall S.empty [] (pureFunc TInt TString)

intCompareTy :: S'
intCompareTy = Forall S.empty [] (pureFunc TInt (pureFunc TInt TInt))

initSession :: Session
initSession = Session 
    { sTrace    = Seq.empty
    , sEnv      = builtinEnv
    , sTEnv     = builtinTEnv
    , sNext     = emptyCounter 0
    , sBatch    = ""
    , sDEnv     = emptyDEnv
    , sAliases  = M.empty
    , sSyns     = M.empty
    , sFields   = M.empty
    , sFixities = M.empty
    , sImp      = []
    }

emptyDEnv :: DEnv
emptyDEnv = DEnv M.empty M.empty S.empty (S.singleton "Console")

emptyCounter :: Int -> Counter
emptyCounter n = Counter n M.empty []

consoleEff :: Eff
consoleEff = Eff (S.singleton "Console") S.empty