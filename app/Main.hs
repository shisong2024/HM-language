{-# LANGUAGE OverloadedStrings #-}
module Main where

import Interp.Eval
import Interp.Parser
import Interp.Pretty
import Interp.Types
import Interp.TypeCheck
import Interp.Builtin

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import System.IO (stdout, hFlush, isEOF)
import Control.Monad.State (runState)
import System.Environment (getArgs)
import Data.Text (Text)
import System.Exit (exitWith, ExitCode (ExitFailure))
import Control.Monad (void, forM_)
import qualified Data.Map as M

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> runFile path
        []     -> do
            TIO.putStrLn "HM interpreter. Type :q to quit."
            loop initSession
        _      -> TIO.putStrLn "Usage: interp [file]"

loop :: Session -> IO ()
loop s = do
    putStr "> "
    hFlush stdout
    done <- isEOF
    if done then TIO.putStrLn "Bye."
    else TIO.getLine >>= \line -> case T.strip line of
        ""   -> loop s
        ":q" -> TIO.putStrLn "Bye."
        src  -> case T.stripPrefix ":t " src of
            Just e  -> loop =<< doTypeOf s e
            Nothing -> case T.stripPrefix ":load " src of
                Just p  -> loop =<< loadFile s (T.unpack (T.strip p))
                Nothing -> case runProg src of
                    Left perr -> TIO.putStrLn ("Parse Error:\n" <> perr) >> loop s
                    Right lprs -> loop =<< execBatch s src lprs []

runFile :: FilePath -> IO ()
runFile path = do
    content <- TIO.readFile path
    case runProg content of
        Left perr -> TIO.putStrLn ("Parse Error:\n" <> perr) >> exitWith (ExitFailure 1)
        Right prs -> void $ execBatch initSession content prs []

execBatch :: Session -> Text -> [(Int, Statement)] -> [(Int, Text)] -> IO Session
execBatch ss src lprs perr = do
    let prs = map snd lprs
        (tsRes, n) = runState (runExceptT (programChecker (sTEnv ss) prs)) (sNext ss)
    case tsRes of
        Left terr -> do
            TIO.putStrLn (T.pack (show (errLine terr)) <> ":")
            TIO.putStrLn ("Type Error:\n" <> prettyTypeErrorWith src terr)
            return ss { sNext = n }
        Right (_, nTEnv, tps) -> do
            let (evRes, _) = runState (runReaderT (runExceptT $ evalProgram prs) (sEnv ss)) (Depth 0)
            case evRes of
                Left eerr -> do
                    TIO.putStrLn ("Eval Error:\n" <> prettyEvalErrorWith src eerr)
                    return ss { sNext = n }
                Right (env', vals) -> do
                    printBatch src lprs (relabel lprs tps) (relabel lprs vals) perr
                    return $ ss
                        { sTEnv = M.union nTEnv (sTEnv ss)
                        , sEnv  = env'
                        , sNext = n
                        }

doTypeOf :: Session -> Text -> IO Session
doTypeOf s e = case runProg e of
    Left perr -> TIO.putStrLn ("Parse Error:\n" <> perr) >> return s
    Right lprs -> do
        let (tcRes, n) = runState (runExceptT (programChecker (sTEnv s) (map snd lprs))) (sNext s)
        case tcRes of
            Left terr -> TIO.putStrLn $ "Type Error:\n" <> prettyTypeErrorWith e terr
            Right (_, _, tps) -> forM_ (M.toAscList tps) $ \(_, info) -> case info of
                TyExpr (Left terr) -> TIO.putStrLn $ "Type Error:\n" <> prettyTypeErrorWith e terr
                TyExpr (Right tp)  -> TIO.putStrLn $ "Type : " <> prettyT' tp
                TyDef na sch        -> TIO.putStrLn $ "def " <> na <> " : " <> prettyS' sch
        return s { sNext = n }

loadFile :: Session -> FilePath -> IO Session
loadFile s path = do
    content <- TIO.readFile path
    case runProg content of
        Left perr -> TIO.putStrLn ("Parse Error:\n" <> perr) >> return s
        Right lprs -> execBatch s content lprs []