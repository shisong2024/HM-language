{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Interp.Eval
import Interp.Parser
import Interp.Pretty
import Interp.Types
import Interp.TypeCheck
import Interp.Builtin
import Interp.Qualify
import Interp.Synonym

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Sequence as Seq

import Control.Monad (forM_, unless, when, foldM)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState, StateT (runStateT))
import Control.Exception (try, IOException)

import System.IO (stdout, hFlush, isEOF)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.Directory (canonicalizePath)
import System.FilePath (takeDirectory, normalise, isAbsolute, (</>))
import System.Info (os)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

import Data.Text (Text)
import Data.Char (toLower)
import Data.Either (isLeft, fromRight)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    setLocaleEncoding utf8
    args <- getArgs
    base <- withPrelude initSession
    case args of
        [path] -> runFile base path
        []     -> do
            TIO.putStrLn "HM interpreter. Type :q to quit."
            loop base base
        _      -> TIO.putStrLn "Usage: interp [file]"

loop :: Session -> Session -> IO ()
loop base s = do
    putStr "> "
    hFlush stdout
    done <- isEOF
    if done then TIO.putStrLn "Bye."
    else TIO.getLine >>= \line -> case T.strip line of
        ""      -> loop base s
        ":q"    -> TIO.putStrLn "Bye."
        ":help" -> help >> loop base s
        ":implicits" -> showImplicits s >> loop base s
        ":trace" -> showTrace s >> loop base s
        ":modules" -> showModules s >> loop base s
        ":reload" -> loop base =<< reloadAll base s
        src
            | Just e <- T.stripPrefix ":t " src -> loop base =<< doTypeOf s e
            | Just p <- T.stripPrefix ":load " src -> loop base =<< loadFile base s (T.unpack (T.strip p))
            | Just a <- T.stripPrefix ":unimport " src ->
                loop base (unloadModule (T.strip (T.dropWhileEnd (== ';') (T.strip a))) s)
            | Just _ <- T.stripPrefix "import " src -> loop base =<< doImport base s src
            | T.isPrefixOf ":" src -> TIO.putStrLn ("Unknown command: " <> src <> ".") >> loop base s
            | otherwise -> case runProgWith (Tabs (sFixities s) (sFields s)) src of
                Left perr -> TIO.putStrLn ("Parse Error:\n" <> perr) >> loop base s
                Right (tb, lprs) -> loop base . fst =<< execBatch True
                    (s { sFixities = tbFix tb, sFields = tbFld tb }) src lprs []

runFile :: Session -> FilePath -> IO ()
runFile base path = do
    (_, ok) <- loadScript True base base path
    unless ok $ exitWith (ExitFailure 1)

-- 批处理：类型检查 + 求值 + 提交。事务性由调用方保证（失败返回输入 session）。
execBatch :: Bool -> Session -> Text -> [(Int, Statement)] -> [(Int, Text)] -> IO (Session, Bool)
execBatch loud ss src lprs perr = case expandProgram (sSyns ss) (map snd lprs) of
    Left msg -> TIO.putStrLn ("Type Error:\n" <> msg) >> return (ss, False)
    Right (syns, prs) ->
        execBatch' syns loud ss src (zip (map fst lprs) prs) perr

execBatch' :: SynTable -> Bool -> Session -> Text -> [(Int, Statement)] -> [(Int, Text)] -> IO (Session, Bool)
execBatch' pendingSyns loud ss src lprs perr = do
    let prs = map snd lprs
        denv = mergeDEnv (buildProgramDEnv prs) (sDEnv ss)
        (tsRes, n) = runState (runExceptT (runReaderT (programChecker (sTEnv ss) (sImp ss) prs) (sDEnv ss))) (sNext ss)
        defBad = \case
            TyDefFailed _ -> True
            TyDefSkipped _ -> True
            _ -> False
    case tsRes of
        Left terr -> do
            TIO.putStrLn (T.pack (show (errLine terr)) <> ":")
            TIO.putStrLn ("Type Error:\n" <> prettyTypeErrorWith (sBatch ss) src terr)
            return (ss { sNext = n }, False)
        Right checked -> do
            let tps = cTypes checked
            if any defBad (M.elems tps) then do
                if loud then printBatch (sBatch ss) src lprs tps M.empty perr
                else mapM_ (\e -> do
                    TIO.putStrLn (T.pack (show (errLine e)) <> ":")
                    TIO.putStrLn ("Type Error:\n" <> prettyTypeErrorWith (sBatch ss) src e)
                    ) [e | TyDefFailed e <- M.elems tps]
                return (ss { sNext = n}, False)
            else do
                let nTEnv = cTEnv checked
                    (resNext, resUsed) = sResume ss
                    initialRuntime = emptyRuntime { rtResumeNext = resNext, rtResumeUsed = resUsed }
                    host "FileIO" "readFile" (VStr path) = do
                        result <- liftIO (try (TIO.readFile (T.unpack path)) :: IO (Either IOException Text))
                        return $ either (left . T.pack . show) (right . VStr) result
                    host "FileIO" "writeFile" (VTuple [VStr path, VStr content]) = do
                        result <- liftIO (try (TIO.writeFile (T.unpack path) content) :: IO (Either IOException ()))
                        return $ either (left . T.pack . show) (const (right (VCtor "Unit" 0 []))) result
                    host eff operation value = defaultHost eff operation value
                    left msg = VCtor "Left" 1 [VStr msg]
                    right v = VCtor "Right" 1 [v]
                (evRes, runtime) <- runStateT (runReaderT (runExceptT (evalCheckedProgramWith host checked)) (sEnv ss)) initialRuntime
                let logged = ss { sTrace = rtLog runtime, sNext = n, sResume = (rtResumeNext runtime, rtResumeUsed runtime)}
                replayOutput (rtLog runtime)
                case evRes of
                    Left eerr -> do
                        TIO.putStrLn ("Eval Error:\n" <> prettyEvalErrorWith (sBatch ss) src eerr)
                        return (logged, False)
                    Right (env', vals) -> do
                        when loud $ printBatch (sBatch ss) src lprs tps vals perr
                        let newCs = concatMap (map fst . dCtors) (dataDeclsOf prs)
                            stale = [c | d <- dataDeclsOf prs, Just cs <- [M.lookup (dName d) (denvDatas (sDEnv ss))]
                                , c <- cs, c `notElem` newCs]
                            newImp = [nm | (i, st) <- zip [0..] prs, StmtImp (Def nm _ _) <- [st], Just (TyDef _ _) <- [M.lookup i tps]]
                            ok   = not (any isLeft (M.elems vals))
                                && not (any (\case TyExpr (Left _) -> True; _ -> False) (M.elems tps))
                        return (logged
                            { sTEnv   = foldr M.delete (M.union nTEnv (sTEnv ss)) stale
                            , sEnv    = foldr M.delete env' stale
                            , sNext   = n
                            , sDEnv   = denv
                            , sFields = M.filter (\fi -> fiCtor fi `notElem` stale) (sFields ss)
                            , sImp    = newImp <> filter (`notElem` newImp) (sImp ss)
                            , sSyns   = pendingSyns
                            }, ok)

doTypeOf :: Session -> Text -> IO Session
doTypeOf s e = case runWith (Tabs (sFixities s) (sFields s)) (T.dropWhileEnd (== ';') (T.strip e)) of
    Left perr -> TIO.putStrLn ("Parse Error:\n" <> perr) >> return s
    Right ex -> case expandProgram (sSyns s) [StmtExpr ex] of
      Left msg -> TIO.putStrLn ("Type Error:\n" <> msg) >> return s
      Right (_, prs) -> case prs of
        [StmtExpr e'] ->do
            let (tcRes, n) = runState (runExceptT (runReaderT (typeOfExprWE (sTEnv s) [sImp s] e') (sDEnv s))) (sNext s)
            case tcRes of
                Left terr -> TIO.putStrLn $ "Type Error:\n" <> prettyTypeErrorWith (sBatch s) e terr
                Right (sch, now) -> do
                    TIO.putStrLn $ "Type: " <> prettyS' sch
                    when (now /= emptyEff) $ TIO.putStrLn ("Effects: " <> prettyEff now)
            return s { sNext = n }
        _ -> do
            let (tcRes, n) = runState (runExceptT (runReaderT (programChecker (sTEnv s) (sImp s) prs) (sDEnv s))) (sNext s)
            case tcRes of
                Left terr -> TIO.putStrLn $ "Type Error:\n" <> prettyTypeErrorWith (sBatch s) e terr
                Right checked ->
                    forM_ (M.toAscList (cTypes checked)) $ \(_, info) -> case info of
                        TyExpr (Left terr) -> TIO.putStrLn $ "Type Error:\n" <> prettyTypeErrorWith (sBatch s) e terr
                        TyExpr (Right tp)  -> TIO.putStrLn $ "Type : " <> prettyT' tp
                        TyDef na sch       -> TIO.putStrLn $ "def " <> na <> " : " <> prettyS' sch
                        TyDefFailed terr   -> TIO.putStrLn $ "Type Error:\n" <> prettyTypeErrorWith (sBatch s) e terr
                        TyDefSkipped _     -> TIO.putStrLn "Not checked."
            return s { sNext = n }

-- ============================================================
-- 脚本模式：`:load` 与 `interp file`。文件内容按顺序并入 session，
-- 文件内的 import 按模块加载（隔离编译）后合并导出。
-- ============================================================

loadFile :: Session -> Session -> FilePath -> IO Session
loadFile base s path = fst <$> loadScript False base s path

loadScript :: Bool -> Session -> Session -> FilePath -> IO (Session, Bool)
loadScript loud base s0 path = do
    src <- readFileSafe path
    case src of
        Left msg -> do
            TIO.putStrLn ("Cannot read " <> T.pack path <> " : " <> msg <> ".")
            return (s0, False)
        Right content -> do
            let dir = dirOf path
            case traverse (parseImportLineText path) (importLines content) of
                Left err -> TIO.putStrLn err >> return (s0, False)
                Right imps -> do
                    (s1, ok1) <- foldM (loadOne dir) (s0, True) imps
                    if not ok1 then return (s1, False) else do
                        let body = blanckImportLines content
                        case runProgWithAt path (Tabs (sFixities s1) (sFields s1)) body of
                            Left perr -> TIO.putStrLn ("Parse Error:\n" <> perr) >> return (s1, False)
                            Right (tb, lprs) -> do
                                (s2, ok2) <- execBatch loud
                                    (s1 { sFixities = tbFix tb, sFields = tbFld tb, sBatch = T.pack path })
                                    body lprs []
                                return (s2, ok2)
  where
    loadOne _ acc@(_, False) _ = return acc
    loadOne dir (s, _) (ImportDecl p a) = do
        resolved <- resolveImportPath dir p
        case checkImportTarget s a resolved of
            Left err -> TIO.putStrLn ("Import Error: " <> err) >> return (s, False)
            Right () -> do
                r <- loadModule loud base [] a p resolved
                case r of
                    Left err -> TIO.putStrLn ("Import Error: " <> err) >> return (s, False)
                    Right (ref, view) -> case applyView True s ref view of
                        Left err -> TIO.putStrLn ("Import Error: " <> err) >> return (s, False)
                        Right s' -> return (s', True)

-- ============================================================
-- 模块模式：import 的加载、导出集合、裁剪与合并。
-- ============================================================

-- | 加载一个模块。base 是 Prelude/builtins 视图（不含调用者定义）。
-- 返回 (模块引用, 只含导出的会话视图)。
loadModule :: Bool -> Session -> [FilePath] -> Text -> FilePath -> FilePath -> IO (Either Text (ModuleRef, Session))
loadModule loud base stack alias display resolved = do
    let key = canonKey resolved
    if key `elem` stack
        then return $ Left $ "cyclic import: " <> T.intercalate " -> " (map T.pack (reverse (key : stack)))
        else do
            src <- readFileSafe resolved
            case src of
                Left msg -> return $ Left ("cannot read " <> T.pack display <> ": " <> msg)
                Right content ->
                    case traverse (parseImportLineText resolved) (importLines content) of
                        Left err -> return (Left err)
                        Right imps -> case firstDup (map importAlias imps) of
                            Just a -> return $ Left ("module " <> T.pack display
                                <> ": import alias `" <> a <> "` appears twice")
                            Nothing -> do
                                let dir = takeDirectory resolved
                                depResults <- mapM (\(ImportDecl p a) -> do
                                                rp <- resolveImportPath dir p
                                                loadModule loud base (key : stack) a p rp) imps
                                case sequence depResults of
                                    Left err -> return (Left err)
                                    Right views
                                        | any (\(ref, _) -> mrAlias ref == alias) views ->
                                            return $ Left ("module " <> T.pack display <> ": import alias `"
                                                <> alias <> "` collides with the module's own alias")
                                        | otherwise -> do
                                            case foldM (\acc (r0, v0) -> applyView False acc r0 v0) base views of
                                                Left err -> return (Left err)
                                                Right compileEnv ->
                                                    compileModule alias display resolved compileEnv content
  where
    compileModule a disp rp env content =
        let body = blanckImportLines content
        in case runProgWithAt disp (Tabs (sFixities env) (sFields env)) body of
            Left perr -> return $ Left perr
            Right (tb, lprs) -> do
                let unq = map snd lprs
                    prs = qualifyProgram a unq
                    env1 = aliasFields (Just a) lprs
                        (env { sFixities = tbFix tb
                             , sFields   = tbFld tb
                             , sBatch    = T.pack disp })
                (modS, ok) <- execBatch loud env1 body (zip (map fst lprs) prs) []
                if not ok
                    then return $ Left ("module " <> T.pack disp <> " did not type check")
                    else do
                        let ex  = moduleExports modS prs
                            ref = ModuleRef a (canonKey rp) disp ex Nothing
                        return $ Right (ref, restrictSession ex modS)

moduleExports :: Session -> Program -> ModuleExports
moduleExports modS prs =
    ModuleExports
        { meVals   = vals
        , meTypes  = tys
        , meCtors  = ctorsAll
        , meEffs   = effs
        , meOps    = opsAll
        , meAbils  = abils
        , meSyns   = ownSyns
        , meFields = fields
        , meFix    = ownOps
        , meImps   = imps
        }
  where
    tops = topsOf prs
    denv = sDEnv modS
    ownDefs = [n | StmtDef (Def n _ _) <- prs] <> [n | StmtImp (Def n _ _) <- prs]
    ownOps = S.fromList [n | n <- ownDefs, isOpName n]
    ops0 = S.fromList [o | StmtOperations d <- prs, (o, _) <- edOperations d]
    ctors = tCtors tops
    types0 = tTypes tops
    ownEffs = S.fromList ([n | StmtEff n <- prs] <> [edName d | StmtOperations d <- prs])
    ownSyns = S.fromList [n | StmtType n _ _ <- prs]
    imps = S.fromList [n | StmtImp (Def n _ _) <- prs]
    vals0 = S.unions [tVals tops, ctors, ops0, ownOps]

    schOf n = M.lookup n (sTEnv modS)
    schemeTypes = S.unions [ts | Just sch <- map schOf (S.toList vals0), let (ts, _) = schemeRefs sch]
    schemeLabels = S.unions [ls | Just sch <- map schOf (S.toList vals0), let (_, ls) = schemeRefs sch]
    synTypes = S.unions [typeRefs (snd t) | n <- S.toList ownSyns, Just t <- [M.lookup n (sSyns modS)]]

    labels = closeLabels (S.union ownEffs schemeLabels)
    closeLabels l =
        let more = S.unions [typeLabels t | n <- S.toList l
                            , Just d <- [M.lookup n (denvOperations denv)]
                            , (_, t) <- edOperations d]
        in if more `S.isSubsetOf` l then l else closeLabels (S.union l more)
    opTypes = S.unions [typeRefs t | n <- S.toList labels
                        , Just d <- [M.lookup n (denvOperations denv)]
                        , (_, t) <- edOperations d]
    opsFromEffs = S.fromList [o | n <- S.toList labels
                            , Just d <- [M.lookup n (denvOperations denv)]
                            , (o, _) <- edOperations d]

    tys = closeTypes denv (sSyns modS) (S.unions [types0, schemeTypes, synTypes, opTypes])
    effs = labels
    opsAll = S.union ops0 opsFromEffs
    ctorsAll = S.union ctors (ctorClosure tys denv)
    vals = S.unions [tVals tops, ctorsAll, opsAll, ownOps]
    abils = S.intersection tys (denvAbilities denv)
    fields = S.fromList [f | (f, fi) <- M.toList (sFields modS)
                        , fiCtor fi `S.member` ctorsAll || fiData fi `S.member` tys]

restrictSession :: ModuleExports -> Session -> Session
restrictSession ex s = s
    { sTEnv     = rk (sTEnv s) (meVals ex)
    , sEnv      = rk (sEnv s) (meVals ex)
    , sDEnv     = (sDEnv s)
        { denvCtors      = rk (denvCtors (sDEnv s)) (meCtors ex)
        , denvDatas      = rk (denvDatas (sDEnv s)) (meTypes ex)
        , denvAbilities  = S.intersection (denvAbilities (sDEnv s)) (meAbils ex)
        , denvEffs       = S.intersection (denvEffs (sDEnv s)) (meEffs ex)
        , denvOperations = rk (denvOperations (sDEnv s)) (meOps ex)
        }
    , sSyns     = rk (sSyns s) (meSyns ex)
    , sFields   = M.filter keepField (sFields s)
    , sFixities = rk (sFixities s) (meFix ex)
    , sImp      = filter (`S.member` meImps ex) (sImp s)
    , sImports  = M.empty
    , sModAlias = M.empty
    }
  where
    rk :: Ord k => M.Map k a -> S.Set k -> M.Map k a
    rk = M.restrictKeys
    keepField fi = fiCtor fi `S.member` meCtors ex || fiData fi `S.member` meTypes ex

applyView :: Bool -> Session -> ModuleRef -> Session -> Either Text Session
applyView register target ref view = do
    when register (checkAlias target ref)
    checkConflicts target ref view
    let merged = target
            { sTEnv     = M.union (sTEnv view) (sTEnv target)
            , sEnv      = M.union (sEnv view) (sEnv target)
            , sDEnv     = DEnv
                { denvCtors      = M.union (denvCtors vd) (denvCtors td)
                , denvDatas      = M.union (denvDatas vd) (denvDatas td)
                , denvAbilities  = S.union (denvAbilities vd) (denvAbilities td)
                , denvEffs       = S.union (denvEffs vd) (denvEffs td)
                , denvOperations = M.union (denvOperations vd) (denvOperations td)
                }
            , sSyns     = M.union (sSyns view) (sSyns target)
            , sFixities = M.union (sFixities view) (sFixities target)
            , sFields   = M.union (sFields view) (sFields target)
            , sImp      = sImp view <> sImp target
            , sNext     = mergeCounter (sNext target) (sNext view)
            , sResume   = mergeResume (sResume target) (sResume view)
            }
    return $ if register
        then merged { sImports  = M.insert (mrAlias ref) ref (sImports target)
                    , sModAlias = M.insert (mrCanonical ref) (mrAlias ref) (sModAlias target) }
        else merged
  where
    vd = sDEnv view
    td = sDEnv target

    checkAlias t r = do
        case M.lookup (mrAlias r) (sImports t) of
            Just old | mrCanonical old /= mrCanonical r ->
                Left ("module alias `" <> mrAlias r <> "` is already used for "
                    <> T.pack (mrPath old) <> "; use `:unimport " <> mrAlias r <> "` first")
            _ -> return ()
        case M.lookup (mrCanonical r) (sModAlias t) of
            Just other | other /= mrAlias r ->
                Left ("module " <> T.pack (mrPath r) <> " is already imported as `" <> other
                    <> "`; use `:unimport " <> other <> "` first")
            _ -> return ()

    checkConflicts t r v = case badFields <> badFix <> badOps of
        (m:_) -> Left m
        []    -> return ()
      where
        where_ = "module `" <> mrAlias r <> "`"
        badFields = ["field `" <> k <> "` from " <> where_
                        <> " conflicts with the field of another module"
                    | (k, fi) <- M.toList (sFields v)
                    , Just old <- [M.lookup k (sFields t)]
                    , old /= fi]
        badFix = ["operator `" <> k <> "` from " <> where_
                    <> " conflicts with an existing operator"
                 | (k, fx) <- M.toList (sFixities v)
                 , Just old <- [M.lookup k (sFixities t)]
                 , old /= fx]
        -- 算子不随模块限定，因此同名算子即使 fixity 相同也必须报冲突
        badOps = ["operator `" <> n <> "` from " <> where_
                    <> " is already defined in this session"
                 | n <- S.toList (meFix (mrExports r))
                 , M.member n (sTEnv t)]

-- | 在加载（编译）模块之前检查别名与身份冲突，避免失败时仍打印模块定义。
checkImportTarget :: Session -> Text -> FilePath -> Either Text ()
checkImportTarget t alias resolved = do
    case M.lookup alias (sImports t) of
        Just old | mrCanonical old /= canonKey resolved ->
            Left ("module alias `" <> alias <> "` is already used for "
                <> T.pack (mrPath old) <> "; use `:unimport " <> alias <> "` first")
        _ -> return ()
    case M.lookup (canonKey resolved) (sModAlias t) of
        Just other | other /= alias ->
            Left ("module " <> T.pack resolved <> " is already imported as `" <> other
                <> "`; use `:unimport " <> other <> "` first")
        _ -> return ()

mergeCounter :: Counter -> Counter -> Counter
mergeCounter a b = a
    { nextId =
        max (nextId a) (nextId b)
    , effDefs = M.union (effDefs a) (effDefs b)
    , effRels = effRels a <> effRels b
    , effTypeSub = M.union (effTypeSub a) (effTypeSub b)
    , effParamRows = S.union (effParamRows a) (effParamRows b)
    }

mergeResume :: (Int, S.Set Int) -> (Int, S.Set Int) -> (Int, S.Set Int)
mergeResume (n1, u1) (n2, u2) = (max n1 n2, S.union u1 u2)

-- | 精确卸载一个模块：按导出集合删除，而不是按名字前缀。
unloadModule :: Text -> Session -> Session
unloadModule a s = case M.lookup a (sImports s) of
    Nothing -> s
    Just ref -> (deleteExports (mrExports ref) s)
        { sImports  = M.delete a (sImports s)
        , sModAlias = M.delete (mrCanonical ref) (sModAlias s)
        }

deleteExports :: ModuleExports -> Session -> Session
deleteExports ex s = s
    { sTEnv     = M.withoutKeys (sTEnv s) (meVals ex)
    , sEnv      = M.withoutKeys (sEnv s) (meVals ex)
    , sDEnv     = (sDEnv s)
        { denvCtors      = M.withoutKeys (denvCtors (sDEnv s)) (meCtors ex)
        , denvDatas      = M.withoutKeys (denvDatas (sDEnv s)) (meTypes ex)
        , denvAbilities  = denvAbilities (sDEnv s) `S.difference` meAbils ex
        , denvEffs       = denvEffs (sDEnv s) `S.difference` meEffs ex
        , denvOperations = M.withoutKeys (denvOperations (sDEnv s)) (meOps ex)
        }
    , sSyns     = M.withoutKeys (sSyns s) (meSyns ex)
    , sFields   = M.filter keep (sFields s)
    , sFixities = M.withoutKeys (sFixities s) (meFix ex)
    , sImp      = filter (`S.notMember` meImps ex) (sImp s)
    }
  where
    keep fi = not (fiCtor fi `S.member` meCtors ex || fiData fi `S.member` meTypes ex)

doImport :: Session -> Session -> Text -> IO Session
doImport base s t = case parseImportLineText "<repl>" t of
    Left err -> TIO.putStrLn err >> return s
    Right (ImportDecl p a) -> do
        resolved <- resolveImportPath "." p
        case M.lookup a (sImports s) of
            Just old | mrCanonical old == canonKey resolved -> do
                let s1 = unloadModule a s
                importInto base s1 a p resolved
            Just old -> do
                TIO.putStrLn ("cannot import `" <> T.pack p <> "` as `" <> a
                    <> "`: alias is already used for " <> T.pack (mrPath old))
                return s
            Nothing -> importInto base s a p resolved
  where
    importInto b target al disp rp = do
        case checkImportTarget target al rp of
            Left err -> TIO.putStrLn ("Import Error: " <> err) >> return target
            Right () -> do
                r <- loadModule False b [] al disp rp
                case r of
                    Left err -> TIO.putStrLn ("Import Error: " <> err) >> return target
                    Right (ref, view) -> case applyView True target ref view of
                        Left err -> TIO.putStrLn ("Import Error: " <> err) >> return target
                        Right s' -> return s'

reloadAll :: Session -> Session -> IO Session
reloadAll base s = do
    let refs = M.toAscList (sImports s)
        cleared = foldr (unloadModule . fst) s refs
    foldM step cleared refs
  where
    step acc (a, ref) = do
        r <- loadModule False base [] a (mrPath ref) (mrCanonical ref)
        case r of
            Left err -> TIO.putStrLn ("Reload Error: " <> err) >> return acc
            Right (ref', view) -> case applyView True acc ref' view of
                Left err -> TIO.putStrLn ("Reload Error: " <> err) >> return acc
                Right s' -> return s'

showModules :: Session -> IO ()
showModules s = do
    TIO.putStrLn "Imported modules (alias -> path):"
    if M.null (sImports s) then TIO.putStrLn "  (empty)"
    else forM_ (M.toAscList (sImports s)) $ \(a, ref) ->
        TIO.putStrLn ("  " <> a <> " -> " <> T.pack (mrPath ref)
            <> "  [" <> T.pack (mrCanonical ref) <> "]")

-- ============================================================
-- 导出集合的辅助函数
-- ============================================================

typeRefs :: T' -> S.Set Text
typeRefs = \case
    TCon n ts -> S.insert n (S.unions (map typeRefs ts))
    TList t -> typeRefs t
    TTuple ts -> S.unions (map typeRefs ts)
    TFunc a e b -> S.unions [typeRefs a, typeRefs b, effTypeRefs e]
    _ -> S.empty

typeLabels :: T' -> S.Set Text
typeLabels = \case
    TFunc a e b -> S.unions [typeLabels a, effLabs e, typeLabels b]
    TList t -> typeLabels t
    TTuple ts -> S.unions (map typeLabels ts)
    TCon _ ts -> S.unions (map typeLabels ts)
    _ -> S.empty

effTypeRefs :: Eff -> S.Set Text
effTypeRefs e = S.unions (map typeRefs (concat (concat (M.elems (effParams e)))))

schemeRefs :: S' -> (S.Set Text, S.Set Text)
schemeRefs sch = (types, labels)
  where
    parts = sType sch : map predType (sPreds sch)
    types = S.unions
        ([S.unions (map typeRefs parts), effTypeRefs (sInitEff sch), effTypeRefs (sDictCall sch)]
         <> [effTypeRefs a <> effTypeRefs b | (a, b) <- sEffCs sch])
    labels = S.unions
        ([S.unions (map typeLabels parts), effLabs (sInitEff sch), effLabs (sDictCall sch)]
         <> [effLabs a <> effLabs b | (a, b) <- sEffCs sch])

closeTypes :: DEnv -> SynTable -> S.Set Text -> S.Set Text
closeTypes denv syns = go
    where
        go s =
            let more = S.unions (map refs (S.toList s))
                s' = S.union s more
            in if s' == s then s else go s'
        refs n = case M.lookup n (denvDatas denv) of
            Just cs -> S.unions
                [ S.unions (map typeRefs (ciArgs ci)) <> typeRefs (ciRes ci)
                | c <- cs, Just ci <- [M.lookup c (denvCtors denv)] ]
            Nothing -> case M.lookup n syns of
                Just (_, t) -> typeRefs t
                Nothing -> S.empty

ctorClosure :: S.Set Text -> DEnv -> S.Set Text
ctorClosure tys denv = S.fromList
    [c | n <- S.toList tys, c <- M.findWithDefault [] n (denvDatas denv)]

firstDup :: Eq a => [a] -> Maybe a
firstDup [] = Nothing
firstDup (x:xs)
    | x `elem` xs = Just x
    | otherwise   = firstDup xs

slash :: FilePath -> FilePath
slash = map (\c -> if c == '\\' then '/' else c)

canonKey :: FilePath -> FilePath
canonKey p = if os == "mingw32" then map toLower (slash p) else slash p

canonicalPath :: FilePath -> IO FilePath
canonicalPath p = do
    r <- try (canonicalizePath p) :: IO (Either IOException FilePath)
    return $ slash $ fromRight (normalise p) r

resolveImportPath :: FilePath -> FilePath -> IO FilePath
resolveImportPath dir p = canonicalPath (if isAbsolute p then p else dir </> p)

readFileSafe :: FilePath -> IO (Either Text Text)
readFileSafe p = do
    r <- try (TIO.readFile p) :: IO (Either IOException Text)
    return $ either (Left . T.pack . show) Right r

replayOutput :: Seq.Seq OutputEvent -> IO ()
replayOutput events = do
    forM_ events $ \case
        WriteText text -> TIO.putStr text
        WriteLine text -> TIO.putStrLn text
    hFlush stdout

showTrace :: Session -> IO ()
showTrace s = do
    TIO.putStrLn "Output trace (last evaluated batch):"
    if Seq.null (sTrace s) then TIO.putStrLn "  (empty)"
    else forM_ (sTrace s) $ \event -> TIO.putStrLn $ "  " <> case event of
        WriteText text -> "putStr " <> prettyV' (VStr text)
        WriteLine text -> "putStrLn " <> prettyV' (VStr text)

showImplicits :: Session -> IO ()
showImplicits s = do
    TIO.putStrLn "Implicit candidates (session frame):"
    if null (sImp s) then TIO.putStrLn "  (empty)"
    else forM_ (sImp s) $ \n -> case M.lookup n (sTEnv s) of
        Just sch -> TIO.putStrLn $ "  " <> n <> " : " <> prettyS' sch
        Nothing  -> TIO.putStrLn ("  " <> n <> " : <missing type>")
    let candidates = [(n, sch) | n <- sImp s, Just sch <- [M.lookup n (sTEnv s)]]
        overlaps =
            [ (a, b) | (i, (a, sa)) <- zip [0 :: Int ..] candidates
            , (b, sb) <- drop (i + 1) candidates
            , candidateHeadsOverlap sa sb]
    unless (null overlaps) $ do
        TIO.putStrLn "Potential head overlaps (same frame; no specificity priority):"
        forM_ overlaps $ \(a, b) -> TIO.putStrLn (" " <> a <> " / " <> b)

help :: IO ()
help = mapM_ TIO.putStrLn
    [ "Commands:"
    , "  :help          show this help"
    , "  :t <expr>      only do type check"
    , "  :load <file>   load file into this session"
    , "  import \"p\" as M  import a module (isolated compilation)"
    , "  :modules       list imported modules"
    , "  :reload        reload all imported modules"
    , "  :unimport M    remove module M from the session"
    , "  :implicits     show implicit candidates"
    , "  :trace         show the last evaluated batch output requests"
    , "  :q             exit"
    ]

preludePath :: FilePath
preludePath = "utils/prelude.txt"

withPrelude :: Session -> IO Session
withPrelude s = do
    r <- readFileSafe preludePath
    case r of
        Left msg -> do
            TIO.putStrLn ("warning: cannot read " <> T.pack preludePath <> " ("
                <> msg <> "); continuing without Prelude")
            return s
        Right tx -> do
            (s', _) <- loadScript False s s preludePath
            case runProgWith (Tabs (sFixities s') (sFields s')) (blanckImportLines tx) of
                Left _ -> return s'
                Right (tb, lprs) ->
                    return $ addPreludePrefix [n | d <- defsOf (map snd lprs), Def n _ _ <- [d]]
                             (s' { sFixities = tbFix tb, sFields = tbFld tb })

aliasFields :: Maybe Text -> [(Int, Statement)] -> Session -> Session
aliasFields ma lprs s = case ma of
    Nothing -> s
    Just a  -> s { sFields = M.map pre (sFields s) }
        where
            ctors, datas :: S.Set Text
            ctors = S.fromList [c | d <- dataDeclsOf (map snd lprs), (c, _) <- dCtors d]
            datas = S.fromList [dName d | d <- dataDeclsOf (map snd lprs)]

            pre :: FieldInfo -> FieldInfo
            pre fi = fi { fiCtor = qua ctors (fiCtor fi), fiData = qua datas (fiData fi) }

            qua :: S.Set Text -> Text -> Text
            qua set n = if n `S.member` set then a <> "." <> n else n

addPreludePrefix :: [Text] -> Session -> Session
addPreludePrefix ns s = s { sTEnv = add (sTEnv s), sEnv = add (sEnv s) }
    where
        add m = M.union (M.mapKeys ("Prelude." <>) (M.restrictKeys m (S.fromList ns))) m

importLines :: Text -> [Text]
importLines t = [T.strip l | l <- T.lines (maskComments t), Just _ <- [T.stripPrefix "import " (T.stripStart l)]]

blanckImportLines :: Text -> Text
blanckImportLines = T.unlines . map blanck . T.lines
    where
        blanck l = case T.stripPrefix "import " (T.stripStart l) of
            Just _ -> ""
            Nothing -> l

dirOf :: FilePath -> FilePath
dirOf p = let (d, _) = T.breakOnEnd "/" (T.pack (slash p)) in if T.null d then "." else T.unpack (T.dropEnd 1 d)
