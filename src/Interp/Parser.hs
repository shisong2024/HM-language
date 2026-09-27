{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Interp.Parser where

import Interp.Types
import Text.Megaparsec 
import Text.Megaparsec.Char (string, char, eol, space, hspace1)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Char (isLower, isAlphaNum, isUpper, isAsciiLower, chr)
import Data.Text (cons, Text, pack, unpack)
import Data.Functor(($>), void)
import Control.Monad (guard, when)
import qualified Data.Text as T

opInfo :: Opr -> (Lev, Assoc)
opInfo = \case
    OArith op -> case op of
        OpAdd -> (1, AssocL)
        OpSub -> (1, AssocL)
        OpMul -> (2, AssocL)
        OpDiv -> (2, AssocL)
        OpPow -> (3, AssocR)
    OCmp _ -> (0, AssocL)

reserved :: [Text]
reserved = ["let", "in", "lambda", "def", "if", "then", "else", "true", "false", "match", "with", "data"]

reservedType :: [Text]
reservedType = ["Int", "Bool"]

isVarFirst :: Char -> Bool
isVarFirst c = isLower c || c == '_'

isVarLeft :: Char -> Bool
isVarLeft c  = isAlphaNum c || c `elem` ['_', '\'']

isOpChar :: Char -> Bool
isOpChar c = c `elem` ("+-*/^=<>!" :: String)

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

parseListLit :: Parser E'
parseListLit = withSpan $ ListLit <$> (lexeme (char '[') *> sepBy parseExpr comma <* lexeme (char ']'))

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

parseParen :: Parser E'
parseParen = withSpan $ do
    _ <- lexeme (char '(')
    ls <- sepBy1 parseExpr comma
    _ <- lexeme (char ')')
    return $ case ls of
        [x] -> x
        es  -> TupleLit es

parseUnary :: Parser E'
parseUnary = withSpan $ do
    m <- optional $ try $ lexeme $ char '-'
    case m of
        Nothing -> parseApp
        Just _  -> do
            e <- parseOpr 3
            case e of
                ILit n -> return $ ILit (-n)
                _      -> return $ BOpr (OArith OpSub) (ILit 0) e

parseAtom :: Parser E'
parseAtom = parseILit <|> parseBLit <|> parseVar <|> parseCtorExpr <|> parseListLit <|> parseParen

parseLet :: Parser E'
parseLet = withSpan $ Let 
    <$> (symbol "let" *> parseVar')
    <*> (lexeme (char '=' <?> "=") *> parseExpr)
    <*> (symbol "in" *> parseExpr)

parseIf :: Parser E'
parseIf = withSpan $ If
    <$> (symbol "if" *> parseExpr)
    <*> (symbol "then" *> parseExpr)
    <*> (symbol "else" *> parseExpr)

parseOpr :: Lev -> Parser E'
parseOpr l = withSpan $ parseUnary >>= \t -> lexeme (loop t)
    where
        loop :: E' -> Parser E'
        loop lhs = do
            mop <- optional $ try $ do
                op <- choice [lexeme (try (string s <* notFollowedBy (satisfy isOpChar))) $> op | (s, op) <- opTable]
                guard (fst (opInfo op) >= l)
                return op
            case mop of
                Nothing -> return lhs
                Just op -> let opt = opInfo op in do
                    let nl = if snd opt == AssocL then fst opt + 1 else fst opt
                    rhs <- parseOpr nl
                    loop (BOpr op lhs rhs)

parseLambda :: Parser E'
parseLambda = withSpan $ do
    _ <- lexeme (symbol "lambda")
    vars <- some parseVar'
    _ <- lexeme (string "->" <?> "->") 
    b <- parseExpr
    return $ foldr Lambda b vars

parseApp :: Parser E'
parseApp = withSpan $ foldl App <$> parseAtom <*> many parseAtom

parseTypeAtom :: [(Text, TypeVar)] -> Parser T'
parseTypeAtom vars = parseTypeParen vars <|> parseTypeList vars <|> parseTypeName vars

parseTypeName :: [(Text, TypeVar)] -> Parser T'
parseTypeName vars = do
    n <- qname <|> varName
    case n of
        "Int" -> return TInt
        "Bool" -> return TBool
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

parseAnn :: Parser E'
parseAnn = do
    e <- parseCons
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

parseExpr :: Parser E'
parseExpr = parseIf <|> parseLet <|> parseLambda <|> parseMatch <|> parseAnn <|> parseCons

parseCtorExpr :: Parser E'
parseCtorExpr = withSpan (try (Var <$> qname) <?> "constructor")

parseDef :: Parser Decl
parseDef = do
    f <- symbol "def" *> parseVar'
    vs <- many parseVar'
    _ <- lexeme (char '=' <?> "=")
    b <- parseExpr
    _ <- semicolon
    return $ Def f vs $ foldr Lambda b vs

parseProg :: Parser [(Int, Statement)]
parseProg = sc *> many 
    (   ((,) . unPos . sourceLine <$> getSourcePos)
    <*> (StmtDef <$> parseDef <|> StmtData <$> parseData <|> StmtExpr <$> (parseExpr <* semicolon)) 
    <*  sc)
    where
        sc :: Parser ()
        sc = skipMany (hspace1 <|> void eol)

run :: Text -> Either Text E'
run t = case runParser (parseExpr <* eof) "" t of
    Left  err -> Left $ pack (errorBundlePretty err)
    Right res -> Right res

stripComment :: Text -> Either Text Text
stripComment t
    | ((i, _): _) <- [(i, l) | (i, l) <- zip [1 :: Int ..] (T.lines t), trailing l] =
        Left $ T.unlines [T.pack (show i) <> ":", "Parse Error:", "`--` must be at the start of a line."]
    | otherwise = Right $ T.unlines (map blanck (T.lines t))
    where
        blanck :: Text -> Text
        blanck l
            | "--" `T.isPrefixOf` T.stripStart l = ""
            | otherwise = l
        
        trailing :: Text -> Bool
        trailing l = let (pre, rest) = T.breakOn "--" l in
            not (T.null rest) && not (T.null (T.strip pre))

runProg :: Text -> Either Text [(Int, Statement)]
runProg tx = case stripComment tx of
    Left msg -> Left msg
    Right t -> case runParser (parseProg <* eof) "" t of
        Left err -> Left $ pack $ errorBundlePretty err
        Right r  -> Right r
    

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
parsePatAtom = parsePatParen <|> parsePatList <|> parsePatCtor <|> parsePatVar <|> parsePatBool <|> parsePatInt

parsePatArg :: Parser P'
parsePatArg = parsePatParen <|> parsePatList <|> parsePatVar <|> parsePatBool <|> parsePatInt <|> (PCtor <$> qname <*> pure [])

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

parseMatch :: Parser E'
parseMatch = withSpan $ Match
    <$> (symbol "match" *> parseExpr <* symbol "with")
    <*> sepBy1 parseArm (lexeme (char '|'))

parseArm :: Parser (P', E')
parseArm = (,) <$> (parsePat <* lexeme (string "->" <?> "->")) <*> parseExpr

parseCons :: Parser E'
parseCons = withSpan $ do
    h <- parseOpr 0
    m <- optional (try (colonTok *> parseCons))
    return $ case m of
        Nothing -> h
        Just t  -> App (App (Var "#cons") h) t

parsePatCtor :: Parser P'
parsePatCtor = PCtor <$> qname <*> many parsePatArg

