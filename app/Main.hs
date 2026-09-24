{-# LANGUAGE OverloadedStrings, LambdaCase #-}
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
import Control.Monad (forM_, unless)
import qualified Data.Map as M
import Data.Either (isLeft)
import Control.Exception (try, IOException)

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
        ""      -> loop s
        ":q"    -> TIO.putStrLn "Bye."
        ":help" -> help >> loop s
        src
            | Just e <- T.stripPrefix ":t " src -> loop =<< doTypeOf s e
            | Just p <- T.stripPrefix ":load " src -> loop =<< loadFile s (T.unpack (T.strip p))
            | T.isPrefixOf ":" src -> TIO.putStrLn ("Unknown command: " <> src <> ".") >> loop s
            | otherwise -> case runProg src of
                Left perr -> TIO.putStrLn ("Parse Error:\n" <> perr) >> loop s
                Right lprs -> loop . fst =<< execBatch s src lprs []

runFile :: FilePath -> IO ()
runFile path = do
    r <- readFileSafe path
    case r of
        Left msg -> TIO.putStrLn ("Cannot read " <> T.pack path <> ": " <> msg)
        Right content -> case runProg content of
            Left perr -> TIO.putStrLn ("Parse Error:\n" <> perr) >> exitWith (ExitFailure 1)
            Right prs -> do
                (_, ok) <- execBatch initSession content prs []
                unless ok $ exitWith (ExitFailure 1)

execBatch :: Session -> Text -> [(Int, Statement)] -> [(Int, Text)] -> IO (Session, Bool)
execBatch ss src lprs perr = do
    let prs = map snd lprs
        (tsRes, n) = runState (runExceptT (programChecker (sTEnv ss) prs)) (sNext ss)
    case tsRes of
        Left terr -> do
            TIO.putStrLn (T.pack (show (errLine terr)) <> ":")
            TIO.putStrLn ("Type Error:\n" <> prettyTypeErrorWith (sBatch ss) src terr)
            return (ss { sNext = n }, False)
        Right (_, nTEnv, tps) -> do
            let (evRes, _) = runState (runReaderT (runExceptT $ evalProgram prs) (sEnv ss)) (Depth 0)
            case evRes of
                Left eerr -> do
                    TIO.putStrLn ("Eval Error:\n" <> prettyEvalErrorWith (sBatch ss) src eerr)
                    return (ss { sNext = n }, False)
                Right (env', vals) -> do
                    printBatch (sBatch ss) src lprs (relabel lprs tps) (relabel lprs vals) perr
                    let ok = not (any isLeft (M.elems vals))
                           && not (any (\case TyExpr (Left _) -> True; _ -> False) (M.elems tps))
                    return (ss { sTEnv = M.union nTEnv (sTEnv ss)
                               , sEnv  = env', sNext = n }, ok)

doTypeOf :: Session -> Text -> IO Session
doTypeOf s e = case runProg e of
    Left perr -> TIO.putStrLn ("Parse Error:\n" <> perr) >> return s
    Right lprs -> do
        let (tcRes, n) = runState (runExceptT (programChecker (sTEnv s) (map snd lprs))) (sNext s)
        case tcRes of
            Left terr -> TIO.putStrLn $ "Type Error:\n" <> prettyTypeErrorWith (sBatch s) e terr
            Right (_, _, tps) -> forM_ (M.toAscList tps) $ \(_, info) -> case info of
                TyExpr (Left terr) -> TIO.putStrLn $ "Type Error:\n" <> prettyTypeErrorWith (sBatch s) e terr
                TyExpr (Right tp)  -> TIO.putStrLn $ "Type : " <> prettyT' tp
                TyDef na sch        -> TIO.putStrLn $ "def " <> na <> " : " <> prettyS' sch
        return s { sNext = n }

loadFile :: Session -> FilePath -> IO Session
loadFile s path = do
    cont <- readFileSafe path
    case cont of
        Left msg -> TIO.putStrLn ("Cannot read " <> T.pack path <> ": " <> msg) >> return s
        Right content -> case runProg content of
            Left perr -> TIO.putStrLn ("Parse Error:\n" <> perr) >> return s
            Right lprs -> fst <$> execBatch s content lprs []

readFileSafe :: FilePath -> IO (Either Text Text)
readFileSafe p = do
    r <- try (TIO.readFile p) :: IO (Either IOException Text)
    return $ either (Left . T.pack . show) Right r

help :: IO ()
help = mapM_ TIO.putStrLn
    [ "Commands:"
    , "  :help         show this help"
    , "  :t <expr>     only do type check"
    , "  :load <file>  load file"
    , "  :q            exit"
    ]