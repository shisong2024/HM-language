{-# LANGUAGE OverloadedStrings, LambdaCase, TupleSections #-}
module Interp.Parser where

import Interp.Types

import qualified Data.Text as T
import qualified Data.Map as M

import Text.Megaparsec
import Text.Megaparsec.Char (string, char, eol, space, hspace1, hexDigitChar)
import Text.Megaparsec.Char.Lexer (decimal)

import Data.Char (isLower, isAlphaNum, isUpper, isAsciiLower, chr, digitToInt)
import Data.Text (cons, Text, pack, unpack)
import Data.Functor(($>), void)

import Control.Monad (guard, when, foldM)
import Data.List (sortOn, nub)
import Data.Either (lefts, rights)
import Data.Maybe (fromMaybe)

import qualified Data.Set as S

opInfo :: Opr -> Fixity
opInfo = \case
    OCmp _ -> Fixity 4 AssocL
    OArith op -> case op of
        OpAdd -> Fixity 6 AssocL
        OpSub -> Fixity 6 AssocL
        OpMul -> Fixity 7 AssocL
        OpDiv -> Fixity 7 AssocL
        OpPow -> Fixity 8 AssocR

isOpChar :: Char -> Bool
isOpChar c = c `elem` ("+-*/^=<>!%&~?" :: String)

isOpName :: Text -> Bool
isOpName n = not (T.null n) && T.all isOpChar n

builtinFix :: FixTab
builtinFix = M.fromList 
    $  [("++", Fixity 5 AssocR)]
    <> [(n, Fixity 4 AssocN) | n <- ["<", ">", "<=", ">="]]

builtinOp :: Text -> Bool
builtinOp n = n `elem` map fst opTable || M.member n builtinFix

defaultFix :: Fixity
defaultFix = Fixity 9 AssocL

reserved :: [Text]
reserved =
    [ "let", "in", "lambda", "def"
    , "if", "then", "else", "true", "false"
    , "match", "with", "data", "type"
    , "infixl", "infixr", "infix"
    , "implicit", "ability", "effect"
    , "handle", "return", "do"
    ]

reservedType :: [Text]
reservedType = ["Int", "Bool", "String"]

isVarFirst :: Char -> Bool
isVarFirst c = isLower c || c == '_'

isVarLeft :: Char -> Bool
isVarLeft c  = isAlphaNum c || c `elem` ['_', '\'']

lexeme :: Parser a -> Parser a
lexeme p = p <* space

nextNotVar :: Parser ()
nextNotVar = notFollowedBy (satisfy isVarLeft)

symbol :: Text -> Parser Text
symbol t = lexeme (try (string t <* nextNotVar))

toBool :: Parser Text -> Parser Bool
toBool p = lexeme $ p >>= \case
        "true"  -> return True
        "false" -> return False
        _       -> fail "expected bool"

comma :: Parser Char
comma = lexeme (char ',')

semicolon :: Parser Char
semicolon = lexeme (char ';')

varIndex :: Text -> Maybe TypeVar
varIndex t = case unpack t of
    [c] | isAsciiLower c -> Just (negate $ fromEnum c)
    _ -> Nothing

parseILit :: Parser E'
parseILit = withSpan $ ILit <$> lexeme (decimal <* nextNotVar <?> "integer literal")

parseBLit :: Parser E'
parseBLit = withSpan $ BLit <$> toBool (symbol "true" <|> symbol "false" <?> "bool")

parseListLit :: Tabs -> Parser E'
parseListLit tab = withSpan $ ListLit <$> (lexeme (char '[') *> sepBy (parseExpr tab) comma <* lexeme (char ']'))

nameRaw :: (Char -> Bool) -> Parser Text
nameRaw f = cons <$> satisfy f <*> takeWhileP Nothing isVarLeft

qname :: Parser Text
qname = lexeme $ do
    a <- nameRaw isUpper
    m <- optional (try (char '.' *> (nameRaw isUpper <|> nameRaw isVarFirst)))
    return $ maybe a (\b -> a <> "." <> b) m

varName :: Parser Text
varName = lexeme $ nameRaw isVarFirst

upperName :: Parser Text
upperName = lexeme $ nameRaw isUpper

parseParam :: Parser Param
parseParam = try parseImpParam <|> (ExplicitParam <$> parseVar')

parseImpParam :: Parser Param
parseImpParam = ImplicitParam
    <$> (lexeme (char '{') *> parseVar')
    <*> (lexeme (string "::" <?> "::") *> parseType <* lexeme (char '}'))

parseVar' :: Parser Text
parseVar' = lexeme $ do
    name <- lookAhead varName
    if name `elem` reserved then fail $ "reserved symbol: " ++ show name
    else varName

parseVar :: Parser E'
parseVar = withSpan (try (Var <$> parseVar') <?> "variable")

parseStrLit :: Parser E'
parseStrLit = withSpan (SLit <$> lexeme strLiteral)

strLiteral :: Parser Text
strLiteral = do
    _ <- char '"'
    cs <- manyTill strChar (char '"' <?> "closing quote")
    return $ T.concat cs

strChar :: Parser Text
strChar = T.singleton <$> (strEscape <|> satisfy plain)
    where
        plain c = c /= '"' && c /= '\\' && c /= '\n' && c /= '\r'

strEscape :: Parser Char
strEscape = do
    _ <- char '\\'
    anySingle >>= \case
        'n'  -> return '\n'
        't'  -> return '\t'
        'r'  -> return '\r'
        '\\' -> return '\\'
        '"'  -> return '"'
        'u'  -> do
            ds <- count 4 hexDigitChar
            return $ chr (foldl (\a d -> a * 16 + digitToInt d) 0 ds)
        c    -> fail $ "unknown escape `\\" <> [c] <> "`"

operatorExpr :: Text -> E' -> E' -> E'
operatorExpr n a b = case lookup n opTable of
    Just op -> BOpr op a b
    Nothing -> App (App (Var n) a) b

operatorValue :: Text -> E'
operatorValue n = case lookup n opTable of
    Just _  -> Lambda "$operator@left" (Lambda "$operator@right"
        (operatorExpr n (Var "$operator@left") (Var "$operator@right")))
    Nothing -> Var n

parseParen :: Tabs -> Parser E'
parseParen tab = withSpan $ do
    start <- getSourcePos
    _ <- lexeme (char '(')
    let binder = "$section@" <> pack (show (unPos (sourceLine start))) 
            <> ":" <> pack (show (unPos (sourceColumn start)))
        reference = operatorValue <$> (opName <* lexeme (char ')'))
        rightSection = do
            n <- opName
            guard (n /= "-")
            rhs <- parseExpr tab <* lexeme (char ')')
            return $ Lambda binder (operatorExpr n (Var binder) rhs)
        ordin = do
            lhs <- parseExpr tab
            (do
                n <- opName <* lexeme (char ')')
                return (Lambda binder (operatorExpr n lhs (Var binder))))
                <|> (do
                    rest <- many (comma *> parseExpr tab)
                    _ <- lexeme (char ')')
                    return $ if null rest then lhs else TupleLit (lhs : rest))
    try reference <|> try rightSection <|> ordin

parseUnary :: Tabs -> Parser E'
parseUnary tab = withSpan $ do
    m <- optional $ try $ lexeme $ char '-'
    case m of
        Nothing -> parseApp tab
        Just _  -> do
            e <- parseOpr (Tabs M.empty (tbFld tab)) 8
            case e of
                ILit n -> return $ ILit (-n)
                _      -> return $ BOpr (OArith OpSub) (ILit 0) e

parseAtom :: Tabs -> Parser E'
parseAtom tab = withSpan (parseAtomHead tab >>= atomTail tab)

parseAtomHead :: Tabs -> Parser E'
parseAtomHead tab = parseILit <|> parseBLit <|> parseStrLit <|> parseVar <|> parseCtorExpr tab <|> parseListLit tab <|> parseParen tab

atomTail :: Tabs -> E' -> Parser E'
atomTail tab e = do
    m <- optional (fieldSel <|> recUpdE tab)
    case m of
        Nothing -> return e
        Just f  -> atomTail tab (f e)

fieldSel :: Parser (E' -> E')
fieldSel = do
    sp <- getSourcePos
    _ <- lexeme (char '.')
    f <- parseVar'
    return $ \e -> App (At (Span sp sp) (Var ("$fld@" <> f))) e

fldBraces :: Parser a -> Parser b -> Parser [(Text, b)]
fldBraces sep p = do
    _ <- lexeme (char '{')
    fs <- sepBy1 fld comma
    _ <- lexeme (char '}')
    return fs
    where
        fld = (,) <$> parseVar' <* lexeme sep <*> p

recUpdE :: Tabs -> Parser (E' -> E')
recUpdE tab = do
    fs <- fldBraces (char '=' <?> "=") (parseExpr tab)
    return $ \e -> App (Var ("$upd@" <> T.intercalate "," (map fst fs)))
                       (TupleLit (e : map snd fs))

parseLet :: Tabs -> Parser E'
parseLet tab = withSpan $ do
    _ <- symbol "let"
    sp0 <- getSourcePos
    off0 <- getOffset
    p <- parsePat tab
    if irrefutableP p then return () else setOffset off0 >> fail refutableLetMsg
    _ <- lexeme (char '=' <?> "=")
    e1 <- parseExpr tab
    _ <- symbol "in"
    e2 <- parseExpr tab
    case p of
        PVar n -> return $ Let n e1 e2
        _ -> let tmp = letTmpName sp0 in return $ Let tmp e1 (Match (Var tmp) [(p, e2)])
    where
        letTmpName :: SourcePos -> Text
        letTmpName sp =
            "$let@" <> pack (show (unPos (sourceLine sp)))
            <> ":" <> pack (show (unPos (sourceColumn sp)))

        refutableLetMsg :: String
        refutableLetMsg =
            "this pattern may fail to match, so it cannot be used as a `let` binding. "
            <> "Bind a variable (or a tuple of variables), or use `match` instead."

parseIf :: Tabs -> Parser E'
parseIf tab = withSpan $ If
    <$> (symbol "if" *> parseExpr tab)
    <*> (symbol "then" *> parseExpr tab)
    <*> (symbol "else" *> parseExpr tab)

parseOpr :: Tabs -> Lev -> Parser E'
parseOpr tab l = withSpan $ parseUnary tab >>= \t -> lexeme (loop t Nothing)
    where
        loop :: E' -> Maybe (Text, Assoc) -> Parser E'
        loop lhs prev = do
            mop <- optional $ try $ do
                (fx, nm, mk) <- choice $
                    [ lexeme (try (string s <* notFollowedBy (satisfy isOpChar))) $> (opInfo op, s, BOpr op)
                    | (s, op) <- opTable ]
                    <> [ lexeme (try (string s <* notFollowedBy (satisfy isOpChar))) $> (fx, s, App . App (Var s))
                       | (s, fx) <- M.toList (tbFix tab) ]
                guard (fixLev fx >= l)
                notFollowedBy (char ')')
                return (fx, nm, mk)
            case mop of
                Nothing -> return lhs
                Just (fx, nm, mk) -> do
                    when (fixAssoc fx == AssocN && maybe False ((== AssocN) . snd) prev) $
                        fail $ "operator `" <> unpack nm <> "` is non-associative; add parentheses."
                    let nl = if fixAssoc fx == AssocR then fixLev fx else fixLev fx + 1
                    rhs <- parseOpr tab nl
                    loop (mk lhs rhs) (Just (nm, fixAssoc fx))

parseLambda :: Tabs -> Parser E'
parseLambda tab = withSpan $ do
    _ <- lexeme (symbol "lambda")
    vars <- some parseVar'
    _ <- lexeme (string "->" <?> "->")
    b <- parseExpr tab
    return $ foldr Lambda b vars

parseApp :: Tabs -> Parser E'
parseApp tab = withSpan $ do
    h <- parseAtom tab
    args <- many (Right <$> parseDictArgs tab <|> Left <$> parseAtom tab)
    let ordin = lefts args
        dicts = concat (rights args)
        names = map daName dicts
    when (length names /= length (nub names)) $
        fail "duplicate named dictionary argument"
    return $  if null dicts then foldl App h ordin else DictCall h ordin dicts

parseDictArgs :: Tabs -> Parser [DictArg]
parseDictArgs tab = do
    _ <- lexeme (char '@') *> lexeme (char '{')
    fields <- sepBy1 field (lexeme (char ','))
    _ <- lexeme (char '}')
    return fields
    where
        field = do
            start <- getSourcePos
            n <- parseVar' <* lexeme (char '=' <?> "=")
            e <- parseExpr tab
            DictArg n e . Span start <$> getSourcePos

parseTypeAtom :: [(Text, TypeVar)] -> Parser T'
parseTypeAtom vars = parseTypeParen vars <|> parseTypeList vars <|> parseTypeName vars

parseTypeName :: [(Text, TypeVar)] -> Parser T'
parseTypeName vars = do
    n <- qname <|> varName
    case n of
        "Int" -> return TInt
        "Bool" -> return TBool
        "String" -> return TString
        _ -> case (lookup n vars, varIndex n) of
            (Just i, _) -> return $ TVar i
            (Nothing, Just i) -> return $ TVar i
            _ -> return (TCon n [])

parseTypeParen :: [(Text, TypeVar)] -> Parser T'
parseTypeParen vars = do
    _ <- lexeme (char '(')
    ts <- sepBy1 (parseTypeWith vars) comma
    _ <- lexeme (char ')')
    return $ case ts of
        [t] -> t
        _ -> TTuple ts

parseTypeList :: [(Text, TypeVar)] -> Parser T'
parseTypeList vars = do
    _ <- lexeme (char '[')
    t <- parseTypeWith vars
    _ <- lexeme (char ']')
    return $ TList t

parseTypeWith :: [(Text, TypeVar)] -> Parser T'
parseTypeWith vars = do
    a <- parseTypeApp vars
    m <- optional $ do
        _ <- lexeme (string "->")
        eff <- option emptyEff (parseEffRowWith vars)
        b <- parseTypeWith vars
        return (TFunc a eff b)
    return $ fromMaybe a m

parseEffRow :: Parser Eff
parseEffRow = parseEffRowWith []

parseEffRowWith :: [(Text, TypeVar)] -> Parser Eff
parseEffRowWith vars = do
    _ <- lexeme (char '{')
    labels <- sepBy ((,) <$> qname <*> many (parseTypeAtom vars)) comma
    _ <- lexeme (char '}')
    return $ EffRow (S.fromList (map fst labels)) S.empty
        (M.fromListWith (++) [(n, [ts]) | (n, ts) <- labels, not (null ts)]) M.empty

parseType :: Parser T'
parseType = parseTypeWith []

parseTypeApp :: [(Text, TypeVar)] -> Parser T'
parseTypeApp vars = do
    h <- parseTypeAtom vars
    rest <- many (parseTypeAtom vars)
    case (h, rest) of
        (TCon n as, rs) -> return (TCon n (as ++ rs))
        (_, []) -> return h
        _ -> fail "type application on a non-type-constructor"

parseAnn :: Tabs -> Parser E'
parseAnn tab = do
    e <- parseCons tab
    m <- optional (try (lexeme (string "::") *> parseType))
    return $ case m of
        Nothing -> e
        Just t -> AnnT e t

parseData :: Parser DataDecl
parseData = symbol "data" *> parseDataCore False

parseAbility :: Parser DataDecl
parseAbility = do
    _ <- symbol "ability"
    d <- parseDataCore True
    case dCtors d of
        [(_, args)] | length args == M.size (dFields d) -> return d
        _ -> fail "an ability must have one constructor (or a nullary maker constructor)"

parseEff :: Parser Statement
parseEff = do
    _ <- symbol "effect"
    n <- upperName
    ps <- many parseVar'
    when (length ps /= length (nub ps)) $ fail "duplicate effect parameter"
    ops <- optional $ do
        _ <- lexeme (char '{')
        os <- sepEndBy1 ((,) <$> parseVar' <* lexeme (string "::") <*> parseTypeWith (zip ps [0..])) comma
        _ <- lexeme (char '}')
        return os
    _ <- semicolon
    case (ps, ops) of
        ([], Nothing) -> return $ StmtEff n
        (_, Just os) -> return $ StmtOperations $ EffDecl n ps os
        _ -> fail "parameterized effects require operation declarations"

parseDo :: Tabs -> Parser E'
parseDo tab = withSpan $ symbol "do" *> lexeme (char '}') *> block
    where
        block = do
            binding <- optional $ try (parseVar' <* lexeme (char '='))
            e <- parseExpr tab
            end <- optional (lexeme (char '}'))
            case end of
                Just _ -> case binding of
                    Nothing -> return e
                    Just _ -> fail "do must end with an expression"
                Nothing -> do
                    _ <- semicolon
                    trailing <- optional (lexeme (char '}'))
                    case trailing of
                        Just _ -> case binding of
                            Nothing -> return e
                            Just _ -> fail "do must end with an expression"
                        Nothing -> StrictLet (fromMaybe "$discard" binding) e <$> block

parseHandle :: Tabs -> Parser E'
parseHandle tab = withSpan $ do
    _ <- symbol "handle"
    n <- qname
    ts <- many (notFollowedBy (symbol "in") *> parseTypeAtom [])
    _ <- symbol "in"
    body <- parseExpr tab
    _ <- symbol "with" *> lexeme (char '{') *> symbol "return"
    ret <- parseVar' <* lexeme (string "->")
    retBody <- parseExpr tab <* semicolon
    ops <- sepEndBy1 arm semicolon
    _ <- lexeme (char '}')
    return $ Handle body n ts ret retBody ops
    where
        arm = (,,,) <$> (qname <|> parseVar') <*> parseVar' <*> parseVar'
            <* lexeme (string "->") <*> parseExpr tab

parseDataCore :: Bool -> Parser DataDecl
parseDataCore sh = do
    n <- upperName
    when (n `elem` reservedType) $
        fail $ unpack $ n <> " is a built in type name and cannot be redeclared."
    ps <- many varName
    case [v | (i, v) <- zip [0..] ps, v `elem` drop (i + 1) ps] of
        (v: _) -> fail $ unpack $ "duplicate type parameter " <> v <> " in data " <> n <> "."
        _ -> return ()
    let vars = zip ps [0..]
    cs <- if sh then (do
        fs <- fldBraces (string "::" <?> "::") (parseTypeWith vars)
        return [(n, map snd fs, map fst fs)]) <|> constructors vars
        else constructors vars
    case [(c, i) | (c, args, _) <- cs, i <- concatMap strayTvs args] of
        [] -> return ()
        ((c, i): _) -> fail $ unpack (T.unwords
            ["constructor", c, "uses type variable", pack [chr (negate i)], "which is not a parameter of", n]
            <> ".")
    case dups [f | (_, _, fs) <- cs, f <- fs] of
        (f: _) -> fail $ unpack $ "duplicate field " <> f <> " in data " <> n <> "."
        [] -> return ()
    _ <- semicolon
    return $ DataDecl n ps [(c, as) | (c, as, _) <- cs]
        (M.fromList [(f, (c, i)) | (c, _, fs) <- cs, (i, f) <- zip [0 :: Int ..] fs])
    where
        constructors vars = lexeme (char '=' <?> "=")
            *> sepBy1 (parseCtorDecl vars) (lexeme (char '|' <?> "|"))

dups :: Eq a => [a] -> [a]
dups xs = [x | (i, x) <- zip [0 :: Int ..] xs, x `elem` drop (i + 1) xs]

parseCtorDecl :: [(Text, TypeVar)] -> Parser (Text, [T'], [Text])
parseCtorDecl vars = do
    n <- upperName
    m <- optional (fldBraces (string "::" <?> "::") (parseTypeWith vars))
    case m of
        Just fs -> return (n, map snd fs, map fst fs)
        Nothing -> do
            as <- many (parseTypeAtom vars)
            return (n, as, [])

parseExpr :: Tabs -> Parser E'
parseExpr tab 
     =  parseDo tab <|> parseHandle tab <|> parseIf tab 
    <|> parseLet tab <|> parseLambda tab <|> parseMatch tab 
    <|> parseAnn tab <|> parseCons tab

parseCtorExpr :: Tabs -> Parser E'
parseCtorExpr tab = withSpan ((try qname >>= ctorExpr tab) <?> "constructor")

ctorExpr :: Tabs -> Text -> Parser E'
ctorExpr tab n = do
    m <- optional (fldBraces (char '=' <?> "=") (parseExpr tab))
    return $ case m of
        Nothing -> Var n
        Just fs -> App (Var ("$rec@" <> n <> "@" <> T.intercalate "," (map fst fs)))
                       (TupleLit (map snd fs))

parseDef :: Tabs -> Parser Decl
parseDef tab = do
    f <- symbol "def" *> defName
    ps <- many parseParam
    _ <- lexeme (char '=' <?> "=")
    b <- parseExpr tab
    _ <- semicolon
    return $ Def f ps b

parseImpDef :: Tabs -> Parser Statement
parseImpDef tab = StmtImp <$> (symbol "implicit" *> parseDef tab)

defName :: Parser Text
defName = parseVar' <|> (lexeme (char '(') *> opName <* lexeme (char ')')) <|> opName

opName :: Parser Text
opName = lexeme (T.pack <$> some (satisfy isOpChar))

parseTypeDecl :: Parser Statement
parseTypeDecl = do
    _ <- symbol "type"
    name <- upperName
    when (name `elem` reservedType) $
        fail (unpack (name <> " is a built-in type name and cannot redeclared"))
    ps <- many varName
    case [v | (i, v) <- zip [0..] ps, v `elem` drop (i + 1) ps] of
        (v: _) ->
            fail $ unpack $ "duplicate type parameter " <> v <> " in type " <> name <> "."
        _ -> return ()
    _ <- lexeme (char '=' <?> "=")
    t <- parseTypeWith (zip ps [0..])
    _ <- semicolon
    return $ StmtType name ps t

parseInfixDecl :: Parser Statement
parseInfixDecl = do
    kw <- lexeme (try ((string "infixl" <|> string "infixr" <|> string "infix") <* nextNotVar))
    lev <- lexeme (oneOf ['0'..'9'] <* nextNotVar <?> "single digit precedence 0-9")
    nm <- opName
    _ <- semicolon
    return $ StmtInfix nm (Fixity (digitToInt lev) (assocOf kw))

assocOf :: Text -> Assoc
assocOf = \case
    "infixl" -> AssocL
    "infixr" -> AssocR
    _        -> AssocN

parseProg :: Tabs -> Parser [(Int, Statement)]
parseProg tab = sc *> many
    (   ((,) . unPos . sourceLine <$> getSourcePos)
    <*> (   parseImpDef tab
        <|> StmtDef <$> parseDef tab
        <|> StmtData <$> parseData
        <|> StmtAbility <$> parseAbility
        <|> parseEff
        <|> parseTypeDecl
        <|> parseInfixDecl
        <|> StmtExpr <$> (parseExpr tab <* semicolon)
        )
    <*  sc)
    where
        sc :: Parser ()
        sc = skipMany (hspace1 <|> void eol)

emptyTabs :: Tabs
emptyTabs = Tabs M.empty M.empty

run :: Text -> Either Text E'
run = runWith emptyTabs

runWith :: Tabs -> Text -> Either Text E'
runWith tab t = case runParser (space *> parseExpr (tab { tbFix = effFix (tbFix tab) }) <* eof) "" (maskComments t) of
    Left  err -> Left $ pack (errorBundlePretty err)
    Right res -> desugarE (tbFld tab) res

effFix :: FixTab -> FixTab
effFix tab = M.union tab builtinFix

maskComments :: Text -> Text
maskComments = T.pack . go False . T.unpack
    where
        go :: Bool -> String -> String
        go _ [] = []
        go True ('\\': c: rest) = '\\': c: go True rest
        go True ('"': rest) = '"' : go False rest
        go False ('"': rest) = '"' : go True rest
        go False ('-': '-': rest) = ' ': ' ': comment rest
        go quoted (c: rest) = c : go quoted rest

        comment :: String -> String
        comment = \case
            [] -> []
            c: rest -> if c == '\n' || c == '\r' then c: go False rest else ' ': comment rest

codeOf :: Text -> Text
codeOf = T.stripEnd . maskComments

stripComment :: Text -> Either Text Text
stripComment = Right . maskComments

sourceStatements :: Text -> [(Int, Text)]
sourceStatements = go False False 1 Nothing [] . T.unpack . maskComments
    where
        go _ _ _ first acc [] = emit first acc []
        go quoted escaped line first acc (c: rest)
            | not quoted && c == ';' = emit first (';': acc) 
                (go False False line Nothing [] rest)
            | otherwise = 
                let nextLine = if c == '\n' then line + 1 else line
                    nextFirst = case first of
                        Nothing | c `notElem` [' ', '\t', '\r', '\n'] -> Just line
                        _ -> first
                    nextQuoted
                        | escaped = quoted
                        | c == '"' = not quoted
                        | otherwise = quoted
                    nextEscaped = quoted && not escaped && c == '\\'
                in go nextQuoted nextEscaped nextLine nextFirst (c: acc) rest
        
        emit Nothing _ rest = rest
        emit (Just f) acc rest = (f, T.pack (reverse acc)): rest
                

irrefutableP :: P' -> Bool
irrefutableP = \case
    PVar _   -> True
    PWild    -> True
    PTuple s -> all irrefutableP s
    _        -> False

runProg :: Text -> Either Text [(Int, Statement)]
runProg = fmap snd . runProgWith emptyTabs

runProgWith :: Tabs -> Text -> Either Text (Tabs, [(Int, Statement)])
runProgWith tab0 tx = case stripComment tx of
    Left msg -> Left msg
    Right t -> case scanFixities (tbFix tab0) t of
        Left msg -> Left msg
        Right fix -> case runParser (parseProg (tab0 { tbFix = effFix fix }) <* eof) "" t of
            Left err -> Left $ pack $ errorBundlePretty err
            Right r  -> do
                (fld, ss) <- recProgram (tbFld tab0) (map snd r)
                return (Tabs fix fld, zip (map fst r) ss)

scanFixities :: FixTab -> Text -> Either Text FixTab
scanFixities tab0 source = do
    let statements = sourceStatements source
    tab <- foldM symDef tab0 statements
    foldM decl tab statements
    where
        symDef tab (_, text) = case runParser (space *> symbol "def" *> defName) "" text of
            Right n | isOpName n && not (builtinOp n) -> 
                Right (M.insertWith (\_ old -> old) n defaultFix tab)
            _ -> Right tab
        
        decl tab (line, text)
            | Just kw <- startsFixity text = case runParser (space *> parseInfixDecl <* eof) "" text of
                Left _ -> Left $ badAt line $ case T.words (T.strip text) of
                    _: level: _ | T.length level /= 1 || T.any (\c -> c < '0' || c > '9') level -> 
                        "the precedence must be a single digit 0-9."
                    [_, _, opToken] | let n = T.dropWhileEnd (== ';') opToken, not (isOpName n) ->
                        "`" <> T.dropWhileEnd (== ';') opToken <> "` is not an operator name."
                    _ -> "expected `" <> kw <> " <0-9> <operator>;`."
                Right (StmtInfix n fx)
                    | fixLev fx > 9 -> Left $ badAt line "the precedecde must be a single digit 0-9."
                    | builtinOp n -> Left $ badAt line ("`" <> n <> "` is a builtin operator.")
                    | otherwise -> Right (M.insert n fx tab)
                Right _ -> Right tab
            | otherwise = Right tab
        
        startsFixity text = case T.words (T.stripStart text) of
            kw: _ | kw `elem` ["infixl", "infixr", "infix"] -> Just kw
            _ -> Nothing

        badAt line mes = "line " <> pack (show line) <> ": " <> mes

withSpan :: Parser E' -> Parser E'
withSpan p = do
    start <- getSourcePos
    expr  <- p
    end   <- getSourcePos
    return $ At (Span start end) expr

strayTvs :: T' -> [TypeVar]
strayTvs = \case
    TInt -> []
    TBool -> []
    TString -> []
    TVar i -> [i | i < 0]
    TList t -> strayTvs t
    TTuple ts -> concatMap strayTvs ts
    TFunc a _ b -> strayTvs a <> strayTvs b
    TCon _ as -> concatMap strayTvs as

---

colonTok :: Parser Char
colonTok = lexeme (char ':' <* notFollowedBy (char ':'))

parsePat :: Tabs -> Parser P'
parsePat tab = do
    h <- parsePatAtom tab
    m <- optional (try (colonTok *> parsePat tab))
    return $ case m of Nothing -> h; Just t -> PCons h t

parsePatAtom :: Tabs -> Parser P'
parsePatAtom tab = parsePatParen tab <|> parsePatList tab <|> parsePatCtor tab <|> parsePatVar <|> parsePatBool <|> parsePatStr <|> parsePatInt

parsePatArg :: Tabs -> Parser P'
parsePatArg tab = parsePatParen tab <|> parsePatList tab <|> parsePatVar <|> parsePatBool <|> parsePatStr <|> parsePatInt <|> (PCtor <$> qname <*> pure [])

parsePatStr :: Parser P'
parsePatStr = PStr <$> lexeme strLiteral

parsePatParen :: Tabs -> Parser P'
parsePatParen tab = do
    _ <- lexeme (char '(')
    ls <- sepBy1 (parsePat tab) comma
    _ <- lexeme (char ')')
    return $ case ls of
        [x] -> x
        es  -> PTuple es

parsePatList :: Tabs -> Parser P'
parsePatList tab = foldr PCons PNil <$> (lexeme (char '[') *> sepBy (parsePat tab) comma <* lexeme (char ']'))

parsePatVar :: Parser P'
parsePatVar = parseVar' >>= \n -> return $ if n == "_" then PWild else PVar n

parsePatInt :: Parser P'
parsePatInt = try $ do
    sign <- optional (lexeme (char '-'))
    n <- lexeme (decimal <* nextNotVar)
    return $ PInt (if sign == Just '-' then negate n else n)

parsePatBool :: Parser P'
parsePatBool = PBool <$> toBool (symbol "true" <|> symbol "false")

parseMatch :: Tabs -> Parser E'
parseMatch tab = withSpan $ Match
    <$> (symbol "match" *> parseExpr tab <* symbol "with")
    <*> sepBy1 (parseArm tab) (lexeme (char '|'))

parseArm :: Tabs -> Parser (P', E')
parseArm tab = (,) <$> (parsePat tab <* lexeme (string "->" <?> "->")) <*> parseExpr tab

parseCons :: Tabs -> Parser E'
parseCons tab = withSpan $ do
    h <- parseOpr tab 0
    m <- optional (try (colonTok *> parseCons tab))
    return $ case m of
        Nothing -> h
        Just t  -> App (App (Var "#cons") h) t

parsePatCtor :: Tabs -> Parser P'
parsePatCtor tab = do
    n <- qname
    m <- optional (fldBraces (char '=' <?> "=") (parsePat tab))
    case m of
        Just fs -> return $ PCtor ("$pat@" <> n <> "@" <> T.intercalate "," (map fst fs))
                                  (map snd fs)
        Nothing -> PCtor n <$> many (parsePatArg tab)

recProgram :: FieldTab -> [Statement] -> Either Text (FieldTab, [Statement])
recProgram tab0 ss = do
    tab <- foldM addDecl tab0 $ dataDeclsOf ss
    ss' <- mapM (recStmt tab) ss
    return (tab, ss')
    where
        addDecl :: FieldTab -> DataDecl -> Either Text FieldTab
        addDecl tab d = foldM (addField d) (dropData (dName d) tab) (M.toList (dFields d))

        dropData :: Text -> FieldTab -> FieldTab
        dropData n = M.filter (\fi -> fiData fi /= n)

        addField :: DataDecl -> FieldTab -> (Text, (Text, Int)) -> Either Text FieldTab
        addField d tab (f, (c, i)) = case M.lookup f tab of
            Just old | not (agrees old new) -> Left $
                "data " <> dName d <> ": field `" <> f <> "` is already a field of `"
                    <> fiCtor old <> "`."
            _ -> Right (M.insert f new tab)
            where
                new = FieldInfo
                    { fiCtor  = c
                    , fiData  = dName d
                    , fiIdx   = i
                    , fiArity = maybe 0 length (lookup c (dCtors d))
                    , fiSolo  = length (dCtors d) == 1
                    }

        agrees :: FieldInfo -> FieldInfo -> Bool
        agrees a b = base (fiCtor a) == base (fiCtor b)
                  && fiIdx a == fiIdx b && fiArity a == fiArity b

        base :: Text -> Text
        base n = snd (T.breakOnEnd "." n)

dataDeclsOf :: Program -> [DataDecl]
dataDeclsOf prs = 
    [ d | stmt <- prs, d <- case stmt of
        StmtData x -> [x]
        StmtAbility x -> [x]
        _ -> []
    ]

recStmt :: FieldTab -> Statement -> Either Text Statement
recStmt tab = \case
    StmtDef d -> StmtDef <$> recDecl tab d
    StmtImp d -> StmtImp <$> recDecl tab d
    StmtExpr e           -> StmtExpr <$> desugarE tab e
    s                    -> Right s
    where
        recDecl :: FieldTab -> Decl -> Either Text Decl
        recDecl t (Def n ps body) = Def n ps <$> desugarE t body

desugarE :: FieldTab -> E' -> Either Text E'
desugarE tab = \case
    ImpHole i       -> Right (ImpHole i)
    ILit i          -> Right (ILit i)
    BLit b          -> Right (BLit b)
    SLit s          -> Right (SLit s)
    Var n           -> Right (Var n)
    ListLit es      -> ListLit <$> mapM (desugarE tab) es
    TupleLit es     -> TupleLit <$> mapM (desugarE tab) es
    Let n u v       -> Let n <$> desugarE tab u <*> desugarE tab v
    StrictLet n u v -> StrictLet n <$> desugarE tab u <*> desugarE tab v
    DictCall h a ds -> DictCall <$> desugarE tab h <*> mapM (desugarE tab) a
        <*> mapM (\d -> desugarE tab (daExpr d) >>= \e -> return d { daExpr = e }) ds
    If b u v        -> If <$> desugarE tab b <*> desugarE tab u <*> desugarE tab v
    BOpr o u v      -> BOpr o <$> desugarE tab u <*> desugarE tab v
    Lambda n b      -> Lambda n <$> desugarE tab b
    AnnT e t        -> AnnT <$> desugarE tab e <*> pure t
    At sp e         -> At sp <$> desugarE tab e
    Match e as      -> Match <$> desugarE tab e
        <*> mapM (\(p, b) -> (,) <$> desugarP tab p <*> desugarE tab b) as
    App f a         -> do
        f' <- desugarE tab f
        a' <- desugarE tab a
        case spanOf f' of
            (sp, Var n) | isMarker n -> recUse tab sp n a'
            _                        -> Right (App f' a')
    Handle b n ts r rb os -> Handle <$> desugarE tab b <*> pure n <*> pure ts <*> pure r
        <*> desugarE tab rb <*> mapM (\(o,x,k,e) -> (o,x,k,) <$> desugarE tab e) os

spanOf :: E' -> (Maybe Span, E')
spanOf = \case
    At sp e -> case spanOf e of
        (Nothing, e') -> (Just sp, e')
        (s, e')       -> (s, e')
    e -> (Nothing, e)

isMarker :: Text -> Bool
isMarker n = any (`T.isPrefixOf` n) ["$fld@", "$rec@", "$upd@", "$pat@"]

recUse :: FieldTab -> Maybe Span -> Text -> E' -> Either Text E'
recUse tab sp n a
    | Just f <- T.stripPrefix "$fld@" n = fieldGet sp tab f a
    | Just r <- T.stripPrefix "$rec@" n = recBuild sp tab r a
    | Just f <- T.stripPrefix "$upd@" n = recUpdate sp tab f a
    | otherwise = Right (App (Var n) a)

fieldGet :: Maybe Span -> FieldTab -> Text -> E' -> Either Text E'
fieldGet sp tab f a = do
    fi <- lookupField sp tab f
    let i  = fiIdx fi
        ar = fiArity fi
        ps = [if j == i then PVar recVal else PWild | j <- [0 .. ar - 1]]
        bad = (PVar recOther, App (App (Var fieldErr) (SLit f)) (Var recOther))
    return $ Match a ((PCtor (fiCtor fi) ps, Var recVal) : [bad | not (fiSolo fi)])

recVal, recOther, fieldErr :: Text
recVal   = "$recVal"
recOther = "$recOther"
fieldErr = "$fieldErr"

lookupField :: Maybe Span -> FieldTab -> Text -> Either Text FieldInfo
lookupField sp tab f =
    maybe (Left $ badSpan sp ("unknown field `" <> f <> "`.")) Right (M.lookup f tab)

ctorFields :: Maybe Span -> FieldTab -> Text -> Either Text [Text]
ctorFields sp tab c =
    case sortOn (fiIdx . snd) [(f, fi) | (f, fi) <- M.toList tab, fiCtor fi == c] of
        [] -> Left $ badSpan sp ("`" <> c <> "` is not a record constructor.")
        fs -> Right (map fst fs)

recBuild :: Maybe Span -> FieldTab -> Text -> E' -> Either Text E'
recBuild sp tab rest a = case T.splitOn "@" rest of
    [c, flds] -> do
        order <- ctorFields sp tab c
        given <- zipFields sp c (T.splitOn "," flds) a
        case [f | (f, _) <- given, f `notElem` order] of
            (f: _) -> Left $ badSpan sp ("`" <> f <> "` is not a field of `" <> c <> "`.")
            [] -> case [f | f <- order, f `notElem` map fst given] of
                (f: _) -> Left $ badSpan sp ("`" <> c <> "` still needs field `" <> f <> "`.")
                [] -> Right $ foldl App (Var c) [v | f <- order, Just v <- [lookup f given]]
    _ -> Left $ badSpan sp "malformed record."

recUpdate :: Maybe Span -> FieldTab -> Text -> E' -> Either Text E'
recUpdate sp tab flds a = case a of
    TupleLit (r: vs) -> do
        given <- zipFields sp "$update" (T.splitOn "," flds) (TupleLit vs)
        fis <- mapM (\(f, _) -> (,) f <$> lookupField sp tab f) given
        let fi = snd (head fis)
        if any ((/= fiCtor fi) . fiCtor . snd) fis
            then Left $ badSpan sp "a record update cannot mix fields of different constructors."
            else do
                let ar = fiArity fi
                    idxs = map (fiIdx . snd) fis
                    valAt j = case [v | ((_, g), v) <- zip fis vs, fiIdx g == j] of
                        (v: _) -> v
                        []     -> Var (oldTmp j)
                    pat = PCtor (fiCtor fi)
                        [if j `elem` idxs then PWild else PVar (oldTmp j) | j <- [0 .. ar - 1]]
                    body = foldl App (Var (fiCtor fi)) [valAt j | j <- [0 .. ar - 1]]
                    bad = (PVar recOther,
                           App (App (Var fieldErr) (SLit (fst (head given)))) (Var recOther))
                return $ Match r ((pat, body) : [bad | not (fiSolo fi)])
    _ -> Left $ badSpan sp "malformed record update."

oldTmp :: Int -> Text
oldTmp j = "$recOld" <> pack (show j)

zipFields :: Maybe Span -> Text -> [Text] -> E' -> Either Text [(Text, E')]
zipFields sp c flds a = case a of
    TupleLit vs
        | length flds /= length vs -> Left $ badSpan sp (malformed c)
        | (d: _) <- dups flds -> Left $ badSpan sp ("field `" <> d <> "` is given twice.")
        | otherwise -> Right (zip flds vs)
    _ -> Left $ badSpan sp (malformed c)
    where
        malformed :: Text -> Text
        malformed n = "malformed record for `" <> n <> "`."

badSpan :: Maybe Span -> Text -> Text
badSpan msp msg = case msp of
    Just (Span st _) -> T.unlines [pack (show (unPos (sourceLine st))) <> ":", msg]
    Nothing          -> msg

desugarP :: FieldTab -> P' -> Either Text P'
desugarP tab = \case
    PCtor n ps | Just rest <- T.stripPrefix "$pat@" n -> patBuild tab rest ps
    PCtor n ps -> PCtor n <$> mapM (desugarP tab) ps
    PCons u v  -> PCons <$> desugarP tab u <*> desugarP tab v
    PTuple ps  -> PTuple <$> mapM (desugarP tab) ps
    p          -> Right p

patBuild :: FieldTab -> Text -> [P'] -> Either Text P'
patBuild tab rest ps = case T.splitOn "@" rest of
    [c, flds] -> do
        let given = if T.null flds then [] else T.splitOn "," flds
        case dups given of
            (d: _) -> Left $ "field `" <> d <> "` is given twice in the same pattern."
            [] -> do
                order <- ctorFields Nothing tab c
                ps' <- mapM (desugarP tab) ps
                case [f | f <- given, f `notElem` order] of
                    (f: _) -> Left $ "`" <> f <> "` is not a field of `" <> c <> "`."
                    [] -> Right $ PCtor c
                        [case [k | (k, g) <- zip [0 :: Int ..] given, g == o] of
                             (k: _) -> ps' !! k
                             []     -> PWild
                        | o <- order]
    _ -> Left "malformed record pattern."
