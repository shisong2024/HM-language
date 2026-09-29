{-# LANGUAGE OverloadedStrings, LambdaCase #-}
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

-- Fixity levels, renumbered into the Haskell 0..9 range. Only the order
-- matters: comparisons 4, `+ -` 6, `* /` 7, `^` 8.
opInfo :: Opr -> Fixity
opInfo = \case
    OCmp _ -> Fixity 4 AssocL
    OArith op -> case op of
        OpAdd -> Fixity 6 AssocL
        OpSub -> Fixity 6 AssocL
        OpMul -> Fixity 7 AssocL
        OpDiv -> Fixity 7 AssocL
        OpPow -> Fixity 8 AssocR

-- `++` is a builtin function, not an `Opr`, so its level cannot come from
-- `opInfo`. `infixr 5` is the level Haskell gives it. The level of anything
-- already built in comes from the interpreter and is not redeclarable.
builtinFix :: FixTab
builtinFix = M.fromList [("++", Fixity 5 AssocR)]

-- Names whose precedence comes from the interpreter rather than the source:
-- the `Opr`s (`opInfo`) and the primitive functions used infix (`builtinFix`).
builtinOp :: Text -> Bool
builtinOp n = n `elem` map fst opTable || M.member n builtinFix

-- What a symbolic `def` name gets when nothing says otherwise, same as an
-- undeclared operator in Haskell.
defaultFix :: Fixity
defaultFix = Fixity 9 AssocL

reserved :: [Text]
reserved = 
    [ "let", "in", "lambda", "def"
    , "if", "then", "else", "true", "false"
    , "match", "with", "data", "type"
    , "infixl", "infixr", "infix"
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

parseListLit :: FixTab -> Parser E'
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

parseVar' :: Parser Text
parseVar' = lexeme $ do
    name <- lookAhead varName
    if name `elem` reserved then fail $ "reserved symbol: " ++ show name 
    else varName

parseVar :: Parser E'
parseVar = withSpan (try (Var <$> parseVar') <?> "variable")

-- `"..."`, with `\n \t \r \\ \"` and `\uXXXX`.
--
-- No raw newline: `stripComment` scans line by line, so a string that
-- spanned lines could hide a `--` from it. `\uXXXX` is the only way to
-- write a non-ASCII character -- source files have to stay pure ASCII.
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

parseParen :: FixTab -> Parser E'
parseParen tab = withSpan $ do
    _ <- lexeme (char '(')
    ls <- sepBy1 (parseExpr tab) comma
    _ <- lexeme (char ')')
    return $ case ls of
        [x] -> x
        es  -> TupleLit es

-- A unary `-` takes only the built-in operators, and of those only `^` is
-- above level 8: `-2 ^ 2` is `-(2 ^ 2)` and `-7 / 2` is `(-7) / 2`, both as
-- before. User operators are left out on purpose, so `-x <+> y` means
-- `(-x) <+> y` whatever level `<+>` was declared at.
parseUnary :: FixTab -> Parser E'
parseUnary tab = withSpan $ do
    m <- optional $ try $ lexeme $ char '-'
    case m of
        Nothing -> parseApp tab
        Just _  -> do
            e <- parseOpr M.empty 8
            case e of
                ILit n -> return $ ILit (-n)
                _      -> return $ BOpr (OArith OpSub) (ILit 0) e

parseAtom :: FixTab -> Parser E'
parseAtom tab = parseILit <|> parseBLit <|> parseStrLit <|> parseVar <|> parseCtorExpr <|> parseListLit tab <|> parseParen tab

parseLet :: FixTab -> Parser E'
parseLet tab = withSpan $ do
    _ <- symbol "let"
    sp0 <- getSourcePos
    off0 <- getOffset
    p <- parsePat
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

parseIf :: FixTab -> Parser E'
parseIf tab = withSpan $ If
    <$> (symbol "if" *> parseExpr tab)
    <*> (symbol "then" *> parseExpr tab)
    <*> (symbol "else" *> parseExpr tab)

-- Precedence climbing. Two tables are in play: `opTable` for the built-in
-- `Opr`s, and the declared operators, which are ordinary function names --
-- `a <+> b` is `(<+>) a b`, so `Eval` and `TypeCheck` never hear about them.
--
-- `prev` is the operator that produced the left operand at this level; it is
-- what makes `a <+> b <+> c` an error when `<+>` is `infix`.
parseOpr :: FixTab -> Lev -> Parser E'
parseOpr tab l = withSpan $ parseUnary tab >>= \t -> lexeme (loop t Nothing)
    where
        loop :: E' -> Maybe (Text, Assoc) -> Parser E'
        loop lhs prev = do
            mop <- optional $ try $ do
                (fx, nm, mk) <- choice $
                    [ lexeme (try (string s <* notFollowedBy (satisfy isOpChar))) $> (opInfo op, s, BOpr op)
                    | (s, op) <- opTable ]
                    <> [ lexeme (try (string s <* notFollowedBy (satisfy isOpChar))) $> (fx, s, App . App (Var s))
                       | (s, fx) <- M.toList tab ]
                guard (fixLev fx >= l)
                return (fx, nm, mk)
            case mop of
                Nothing -> return lhs
                Just (fx, nm, mk) -> do
                    when (fixAssoc fx == AssocN && maybe False ((== AssocN) . snd) prev) $
                        fail $ "operator `" <> unpack nm <> "` is non-associative; add parentheses."
                    let nl = if fixAssoc fx == AssocR then fixLev fx else fixLev fx + 1
                    rhs <- parseOpr tab nl
                    loop (mk lhs rhs) (Just (nm, fixAssoc fx))

parseLambda :: FixTab -> Parser E'
parseLambda tab = withSpan $ do
    _ <- lexeme (symbol "lambda")
    vars <- some parseVar'
    _ <- lexeme (string "->" <?> "->") 
    b <- parseExpr tab
    return $ foldr Lambda b vars

parseApp :: FixTab -> Parser E'
parseApp tab = withSpan $ foldl App <$> parseAtom tab <*> many (parseAtom tab)

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
    ts <- sepBy1 (parseTypeApp vars) (lexeme (string "->" <?> "?"))
    return $ case ts of
        [t] -> t
        _ -> foldr1 TFunc ts

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

parseAnn :: FixTab -> Parser E'
parseAnn tab = do
    e <- parseCons tab
    m <- optional (try (lexeme (string "::") *> parseType))
    return $ case m of
        Nothing -> e
        Just t -> AnnT e t

parseData :: Parser DataDecl
parseData = do
    _ <- symbol "data"
    n <- upperName
    when (n `elem` reservedType) $
        fail $ unpack $ n <> " is a built in type name and cannot be redeclared."
    ps <- many varName
    case [v | (i, v) <- zip [0..] ps, v `elem` drop (i + 1) ps] of
        (v: _) -> fail $ unpack $ "duplicate type parameter " <> v <> " in data " <> n <> "."
        _ -> return ()
    let vars = zip ps [0..]
    _ <- lexeme (char '=' <?> "=")
    cs <- sepBy1 (parseCtorDecl vars) (lexeme (char '|' <?> "|"))
    case [(c, i) | (c, args) <- cs, i <- concatMap strayTvs args] of
        [] -> return ()
        ((c, i): _) -> fail $ unpack (T.unwords
            ["constructor", c, "uses type variable", pack [chr (negate i)], "which is not a parameter of", n]
            <> ".")
    _ <- semicolon
    return $ DataDecl n ps cs

parseCtorDecl :: [(Text, TypeVar)] -> Parser (Text, [T'])
parseCtorDecl vars = (,) <$> upperName <*> many (parseTypeAtom vars)

parseExpr :: FixTab -> Parser E'
parseExpr tab = parseIf tab <|> parseLet tab <|> parseLambda tab <|> parseMatch tab <|> parseAnn tab <|> parseCons tab

parseCtorExpr :: Parser E'
parseCtorExpr = withSpan (try (Var <$> qname) <?> "constructor")

-- `def (<+>) a b = ...` and `def <+> a b = ...` both name the function
-- `<+>`, which is what `a <+> b` calls. Such a name is never qualified --
-- there is no way to write `M.<+>` -- so a declared operator is visible in
-- every module that is loaded.
parseDef :: FixTab -> Parser Decl
parseDef tab = do
    f <- symbol "def" *> defName
    vs <- many parseVar'
    _ <- lexeme (char '=' <?> "=")
    b <- parseExpr tab
    _ <- semicolon
    return $ Def f vs $ foldr Lambda b vs

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

-- `infixl 6 <+>;` -- only the shape is recognised here. Which names are
-- legal is settled by `scanFixities`, which reads the text first and is the
-- one that knows the line numbers.
parseInfixDecl :: Parser Statement
parseInfixDecl = do
    kw <- lexeme (try ((string "infixl" <|> string "infixr" <|> string "infix") <* nextNotVar))
    lev <- lexeme (decimal <* nextNotVar <?> "precedence")
    nm <- opName
    _ <- semicolon
    return $ StmtInfix nm (Fixity (fromInteger lev) (assocOf kw))

assocOf :: Text -> Assoc
assocOf = \case
    "infixl" -> AssocL
    "infixr" -> AssocR
    _        -> AssocN

parseProg :: FixTab -> Parser [(Int, Statement)]
parseProg tab = sc *> many 
    (   ((,) . unPos . sourceLine <$> getSourcePos)
    <*> (   StmtDef <$> parseDef tab 
        <|> StmtData <$> parseData 
        <|> parseTypeDecl 
        <|> parseInfixDecl
        <|> StmtExpr <$> (parseExpr tab <* semicolon)
        ) 
    <*  sc)
    where
        sc :: Parser ()
        sc = skipMany (hspace1 <|> void eol)

run :: Text -> Either Text E'
run = runWith M.empty

runWith :: FixTab -> Text -> Either Text E'
runWith tab t = case runParser (parseExpr (effFix tab) <* eof) "" t of
    Left  err -> Left $ pack (errorBundlePretty err)
    Right res -> Right res

-- The table a parse really uses: the declared operators plus the built-in
-- `++` (which no declaration can collide with).
effFix :: FixTab -> FixTab
effFix tab = M.union tab builtinFix

-- Everything up to a `--` that is not inside a string literal. A backslash
-- inside a string escapes the next character, so `"a\"--b"` is one string.
codeOf :: Text -> Text
codeOf = T.pack . go False . T.unpack
    where
        go :: Bool -> String -> String
        go _ [] = []
        go True ('\\': c: rest) = '\\' : c : go True rest
        go True ('"': rest) = '"' : go False rest
        go False ('"': rest) = '"' : go True rest
        go False ('-': '-': _) = []
        go inStr (c: rest) = c : go inStr rest

stripComment :: Text -> Either Text Text
stripComment t
    | ((i, _): _) <- [(i, l) | (i, l) <- zip [1 :: Int ..] (T.lines t), trailing l] =
        Left $ T.unlines [T.pack (show i) <> ":", "Parse Error:", "`--` must be at the start of a line."]
    | otherwise = Right $ T.unlines (map blanck (T.lines t))
    where
        blanck :: Text -> Text
        blanck l
            | commented l && T.null (T.strip (codeOf l)) = ""
            | otherwise = l

-- A `--` outside a string cuts the line: everything from there on is a
-- comment. A trailing comment is still an error (stage 7 lifts that).
        commented :: Text -> Bool
        commented l = codeOf l /= l

        trailing :: Text -> Bool
        trailing l = commented l && not (T.null (T.strip (codeOf l)))

irrefutableP :: P' -> Bool
irrefutableP = \case
    PVar _   -> True
    PWild    -> True
    PTuple s -> all irrefutableP s
    _        -> False

runProg :: Text -> Either Text [(Int, Statement)]
runProg = fmap snd . runProgWith M.empty

runProgWith :: FixTab -> Text -> Either Text (FixTab, [(Int, Statement)])
runProgWith tab0 tx = case stripComment tx of
    Left msg -> Left msg
    Right t -> case scanFixities tab0 t of
        Left msg -> Left msg
        Right tab -> case runParser (parseProg (effFix tab) <* eof) "" t of
            Left err -> Left $ pack $ errorBundlePretty err
            Right r  -> Right (tab, r)

-- Fixity has to be known while the expressions are being read, so it is
-- collected from the text before the parser runs: a declaration further down
-- the file is in force for the lines above it, the same as anywhere else.
--
-- Two passes: a symbolic `def` name registers at the default level (that is
-- the only way to learn an operator exists), then the `infixl`/`infixr`/
-- `infix` lines set the real ones. Both passes consult `tab0`, so operators
-- from earlier REPL lines and already-loaded modules are in scope too.
scanFixities :: FixTab -> Text -> Either Text FixTab
scanFixities tab0 t = do
    tab <- foldM symDef tab0 (zip [1 :: Int ..] ls)
    foldM decl tab (zip [1 :: Int ..] ls)
    where
        ls :: [Text]
        ls = T.lines t

        -- `;` is dropped wherever it sits, so `infixl 6 <+>;` and
        -- `def (<+>) a b = ...` both come down to plain words.
        wordsOf :: Text -> [Text]
        wordsOf = filter (/= ";") . T.words . T.strip

        bare :: Text -> Text
        bare w = case T.stripPrefix "(" w of
            Just r  -> T.dropWhileEnd (== ')') r
            Nothing -> T.dropWhileEnd (== ';') w

        symDef :: FixTab -> (Int, Text) -> Either Text FixTab
        symDef tab (_, l) = case wordsOf l of
            ("def": w: _)
                | let nm = bare w
                , isOpName nm && nm `notElem` map fst opTable
                -> Right (M.insertWith (\_ old -> old) nm defaultFix tab)
            _ -> Right tab

        decl :: FixTab -> (Int, Text) -> Either Text FixTab
        decl tab (i, l) = case wordsOf l of
            (kw: rest) | kw `elem` ["infixl", "infixr", "infix"] -> do
                (nm, fx) <- declOf i kw rest
                return $ M.insert nm fx tab
            _ -> Right tab

        declOf :: Int -> Text -> [Text] -> Either Text (Text, Fixity)
        declOf i kw rest = case rest of
            [lv, w] -> do
                lev <- case T.unpack lv of
                    [c] | c >= '0' && c <= '9' -> Right (fromEnum c - fromEnum '0')
                    _ -> Left $ badAt i "the precedence must be a single digit 0-9."
                let nm = bare w
                if not (isOpName nm)
                    then Left $ badAt i ("`" <> nm <> "` is not an operator name.")
                    else if builtinOp nm
                        then Left $ badAt i ("`" <> nm <> "` is a built-in operator; its precedence is fixed.")
                        else Right (nm, Fixity lev (assocOf kw))
            _ -> Left $ badAt i "expected `infixl <0-9> <operator>;`."

        badAt :: Int -> Text -> Text
        badAt i msg = T.unlines [T.pack (show i) <> ":", "Parse Error:", msg]
    

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
    TFunc a b -> strayTvs a <> strayTvs b
    TCon _ as -> concatMap strayTvs as

---

colonTok :: Parser Char
colonTok = lexeme (char ':' <* notFollowedBy (char ':'))

parsePat :: Parser P'
parsePat = do
    h <- parsePatAtom
    m <- optional (try (colonTok *> parsePat))
    return $ case m of Nothing -> h; Just t -> PCons h t

parsePatAtom :: Parser P'
parsePatAtom = parsePatParen <|> parsePatList <|> parsePatCtor <|> parsePatVar <|> parsePatBool <|> parsePatStr <|> parsePatInt

parsePatArg :: Parser P'
parsePatArg = parsePatParen <|> parsePatList <|> parsePatVar <|> parsePatBool <|> parsePatStr <|> parsePatInt <|> (PCtor <$> qname <*> pure [])

parsePatStr :: Parser P'
parsePatStr = PStr <$> lexeme strLiteral

parsePatParen :: Parser P'
parsePatParen = do
    _ <- lexeme (char '(')
    ls <- sepBy1 parsePat comma
    _ <- lexeme (char ')')
    return $ case ls of
        [x] -> x
        es  -> PTuple es

parsePatList :: Parser P'
parsePatList = foldr PCons PNil <$> (lexeme (char '[') *> sepBy parsePat comma <* lexeme (char ']'))

parsePatVar :: Parser P'
parsePatVar = parseVar' >>= \n -> return $ if n == "_" then PWild else PVar n

parsePatInt :: Parser P'
parsePatInt = try $ do
    sign <- optional (lexeme (char '-'))
    n <- lexeme (decimal <* nextNotVar)
    return $ PInt (if sign == Just '-' then negate n else n)

parsePatBool :: Parser P'
parsePatBool = PBool <$> toBool (symbol "true" <|> symbol "false")

parseMatch :: FixTab -> Parser E'
parseMatch tab = withSpan $ Match
    <$> (symbol "match" *> parseExpr tab <* symbol "with")
    <*> sepBy1 (parseArm tab) (lexeme (char '|'))

parseArm :: FixTab -> Parser (P', E')
parseArm tab = (,) <$> (parsePat <* lexeme (string "->" <?> "->")) <*> parseExpr tab

parseCons :: FixTab -> Parser E'
parseCons tab = withSpan $ do
    h <- parseOpr tab 0
    m <- optional (try (colonTok *> parseCons tab))
    return $ case m of
        Nothing -> h
        Just t  -> App (App (Var "#cons") h) t

parsePatCtor :: Parser P'
parsePatCtor = PCtor <$> qname <*> many parsePatArg

