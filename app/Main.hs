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
import qualified Data.Map as M
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import System.IO (stdout, hFlush, isEOF)
import Control.Monad.State (runState)
import System.Environment (getArgs)
import Data.Text (Text)
import System.Exit (exitWith, ExitCode (ExitFailure))
import Control.Monad (forM_, unless, when, foldM)
import Data.Either (isLeft)
import Control.Exception (try, IOException)
import Data.Char (isAlphaNum, isUpper)
import Interp.Qualify (qualifyProgram)
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> runFile path
        []     -> do
            TIO.putStrLn "HM interpreter. Type :q to quit."
            loop =<< withPrelude initSession
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
            | Just _ <- T.stripPrefix "import " src -> loop =<< doImport s src
            | T.isPrefixOf ":" src -> TIO.putStrLn ("Unknown command: " <> src <> ".") >> loop s
            | otherwise -> case runProg src of
                Left perr -> TIO.putStrLn ("Parse Error:\n" <> perr) >> loop s
                Right lprs -> loop . fst =<< execBatch True s src lprs []

runFile :: FilePath -> IO ()
runFile path = do
    s0 <- withPrelude initSession
    (_, ok) <- loadWithImports [] True s0 Nothing path
    unless ok $ exitWith (ExitFailure 1)

execBatch :: Bool -> Session -> Text -> [(Int, Statement)] -> [(Int, Text)] -> IO (Session, Bool)
execBatch loud ss src lprs perr = do
    let prs = map snd lprs
        denv = unionDEnv (buildDEnv (dataDeclsOf prs)) (sDEnv ss)
        (tsRes, n) = runState (runExceptT (runReaderT (programChecker (sTEnv ss) prs) denv)) (sNext ss)
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
                    when loud $ printBatch (sBatch ss) src lprs (relabel lprs tps) (relabel lprs vals) perr
                    let newCs = concatMap (map fst . dCtors) (dataDeclsOf prs)
                        stale = [c | d <- dataDeclsOf prs
                                   , Just cs <- [M.lookup (dName d) (denvDatas (sDEnv ss))]
                                   , c <- cs, c `notElem` newCs
                                ]
                        ok = not (any isLeft (M.elems vals))
                           && not (any (\case TyExpr (Left _) -> True; _ -> False) (M.elems tps))
                    return (ss { sTEnv = foldr M.delete (M.union nTEnv (sTEnv ss)) stale
                               , sEnv  = foldr M.delete env' stale
                               , sNext = n, sDEnv = denv }, ok)

doTypeOf :: Session -> Text -> IO Session
doTypeOf s e = case runProg e of
    Left perr -> TIO.putStrLn ("Parse Error:\n" <> perr) >> return s
    Right lprs -> do
        let denv = unionDEnv (buildDEnv (dataDeclsOf (map snd lprs))) (sDEnv s)
            (tcRes, n) = runState (runExceptT (runReaderT (programChecker (sTEnv s) (map snd lprs)) denv)) (sNext s)
        case tcRes of
            Left terr -> TIO.putStrLn $ "Type Error:\n" <> prettyTypeErrorWith (sBatch s) e terr
            Right (_, _, tps) -> forM_ (M.toAscList tps) $ \(_, info) -> case info of
                TyExpr (Left terr) -> TIO.putStrLn $ "Type Error:\n" <> prettyTypeErrorWith (sBatch s) e terr
                TyExpr (Right tp)  -> TIO.putStrLn $ "Type : " <> prettyT' tp
                TyDef na sch        -> TIO.putStrLn $ "def " <> na <> " : " <> prettyS' sch
        return s { sNext = n }

loadFile :: Session -> FilePath -> IO Session
loadFile s path = fst <$> loadWithImports [] False s Nothing path

-- REPL 版的 import：语法与文件里那几行**完全一致**（复用 parseImportLine，
-- 于是 R1-R4 的报错措辞自动一致）。区别只有一点：这里 `as` 是必需的 ——
-- 不带别名的裸加载继续走 `:load`（它是「整体不加前缀」的逃生口）。
doImport :: Session -> Text -> IO Session
doImport s t = case parseImportLine "<repl>" t of
    Left err -> TIO.putStrLn err >> return s
    Right (p, a) -> fst <$> loadWithImports [] False s (Just a) p

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

unionDEnv :: DEnv -> DEnv -> DEnv
unionDEnv a b = 
    let redef = M.keys (M.intersection (denvDatas a) (denvDatas b))
        stale = [c | n <- redef, Just cs <- [M.lookup n (denvDatas b)], c <- cs] in
    DEnv (M.union (denvCtors a) (foldr M.delete (denvCtors b) stale)) (M.union (denvDatas a) (denvDatas b))

preludePath :: FilePath
preludePath = "utils/prelude.txt"

withPrelude :: Session -> IO Session
withPrelude s = do
    r <- readFileSafe preludePath
    case r of
        Left _ -> return s
        Right tx -> do
            s' <- loadFile s preludePath
            case runProg (blanckImportLines tx) of
                Left _ -> return s'
                Right lprs -> 
                    return $ addPreludePrefix [n | (_, StmtDef (Def n _ _)) <- lprs] s'

addPreludePrefix :: [Text] -> Session -> Session
addPreludePrefix ns s = s { sTEnv = add (sTEnv s), sEnv = add (sEnv s) }
    where
        add m = M.union (M.mapKeys ("Prelude." <>) (M.restrictKeys m (S.fromList ns))) m

importLines :: Text -> [Text]
importLines t = [T.strip l | l <- T.lines t, Just _ <- [T.stripPrefix "import " (T.stripStart l)]]

parseImportLine :: FilePath -> Text -> Either Text (FilePath, Text)
parseImportLine file l0 = do
    let bad msg = T.pack file <> ": " <> msg
        l = T.strip (T.dropWhileEnd (== ';') (T.strip l0))
        unq t = fromMaybe t (T.stripSuffix "\"" =<< T.stripPrefix "\"" t)
    case T.words (T.strip l) of
        ["import", _] -> Left $ bad "needs `as <Alias>`"
        ["import", p, "as", a]
            | not (upperAlias a) -> 
                Left $ bad ("module alias `" <> a <> "` must start with an uppercase letter")
            | a == "Prelude" -> Left $ bad "`Prelude` is reserved for the prelude"
            | otherwise -> Right (T.unpack (unq p), a)
        _ -> Left $ bad ("malformed import line: " <> T.strip l)

upperAlias :: Text -> Bool
upperAlias a = case T.uncons a of
    Just (c, r) -> isUpper c && T.all (\x -> isAlphaNum x || x `elem` ['_', '\'']) r
    Nothing -> False

blanckImportLines :: Text -> Text
blanckImportLines = T.unlines . map blanck . T.lines
    where
        blanck l = case T.stripPrefix "import " (T.stripStart l) of
            Just _ -> ""
            Nothing -> l

dirOf :: FilePath -> FilePath
dirOf p = let (d, _) = T.breakOnEnd "/" (T.pack p) in if T.null d then "." else T.unpack (T.dropEnd 1 d)

resolve :: FilePath -> FilePath -> FilePath
resolve dir p
    | "/" `T.isPrefixOf` T.pack p = p
    | dir == "." = p
    | otherwise = dir <> "/" <> p

loadWithImports :: [FilePath] -> Bool -> Session -> Maybe Text -> FilePath -> IO (Session, Bool)
loadWithImports seen loud s malias path
    | path `elem` seen = return (s, True)
    | Just a <- malias
    , Just p <- M.lookup a (sAliases s)
    = if p == path 
        then return (s, True) 
        else bail ("alias `" <> a <> "` is already used for " <> T.pack p)
    | Just _ <- malias
    , Just a0 <- aliasOf s path
    = bail ("module " <> T.pack path <> " is already imported as `" <> a0 <> "`")
    | otherwise = do
        r <- readFileSafe path
        case r of
            Left msg -> TIO.putStrLn ("Cannot read " <> T.pack path <> " : " <> msg <> ".") >> return (s, False)
            Right content -> do
                let dir = dirOf path
                    body = blanckImportLines content
                case traverse (parseImportLine path) (importLines content) of
                    Left err -> bail err
                    Right imps -> do
                        (s1, ok1) <- foldM (\(sa, oka) (p, a) -> do
                            (sb, okb) <- loadWithImports (path: seen) loud sa (Just a) (resolve dir p)
                            return (sb, oka && okb)) 
                            (s, True) imps
                        case runProg body of
                            Left perr -> TIO.putStrLn ("Parse Error:\n" <> perr) >> return (s1, False)
                            Right lprs -> do
                                let sndl = map snd lprs
                                    prs = case malias of
                                        Nothing -> sndl
                                        Just a -> qualifyProgram a sndl
                                    lprs' = zip (map fst lprs) prs
                                (s2, ok2) <- execBatch loud s1 body lprs' []
                                let s3 = maybe s2 (\a -> s2 { sAliases = M.insert a path (sAliases s2) }) malias
                                return (s3, ok2 && ok1)
    where
        bail :: Text -> IO (Session, Bool)
        bail msg = TIO.putStrLn msg >> return (s, False)

aliasOf :: Session -> FilePath -> Maybe Text
aliasOf s p = case [a | (a, p') <- M.toList (sAliases s), p' == p] of
    (a: _) -> Just a
    [] -> Nothing