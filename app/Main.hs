{-# LANGUAGE OverloadedStrings #-}
module Main where

import Interp.Eval
import Interp.Parser
import Interp.Pretty

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Control.Monad.State (runState)
import Interp.TypeCheck (typeChecker)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> runFile path
        []     -> do
            TIO.putStrLn "HM interpreter. Type :q to quit."
            loop
        _      -> TIO.putStrLn "Usage: interp [file]"

loop ::IO ()
loop = do
    putStr "> "
    hFlush stdout
    line <- TIO.getLine
    case T.strip line of
        ""   -> loop
        ":q" -> TIO.putStrLn "Bye."
        src  -> processLine src >> loop

processLine :: T.Text -> IO ()
processLine src = case run src of
    Left perr -> TIO.putStrLn ("Parse error:\n" <> perr)
    Right expr -> do
        let (tcResult, _) =
                runState (runExceptT (typeChecker emptyMap expr)) 0
        case tcResult of
            Left terr -> TIO.putStrLn ("Type error: " <> prettyTypeError terr)
            Right (_, ty) -> do
                TIO.putStrLn ("Type: " <> prettyT' ty)
                let evResult = runReaderT (eval expr) emptyMap
                case evResult of
                    Left eerr -> TIO.putStrLn ("Eval error:" <> prettyEvalError eerr)
                    Right v   -> TIO.putStrLn ("Value:" <> prettyV' v)

runFile :: FilePath -> IO ()
runFile path = do
    content <- TIO.readFile path
    mapM_ processLine' (zip [1..] (T.lines content))
  where
    processLine' :: (Int, T.Text) -> IO ()
    processLine' (n, line) = do
        let src = T.strip line
        if T.null src then return ()
        else TIO.putStr (T.pack (show n) <> ": ") >> processLine src