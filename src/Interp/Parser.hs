{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Interp.Parser where

import Interp.Types
import Text.Megaparsec ((<?>), (<|>), satisfy, MonadParsec (takeWhileP, eof, try, lookAhead), notFollowedBy, runParser, errorBundlePretty, choice, optional, many, between, getSourcePos, some, skipMany, unPos, SourcePos (sourceLine))
import Text.Megaparsec.Char (string, char, eol, hspace, hspace1)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Char (isLower, isAlphaNum)
import Data.Text (cons, Text, pack)
import Data.Functor(($>), void)
import Control.Monad (guard)

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
reserved = ["let", "in", "lambda", "def", "if", "then", "else", "true", "false"]

isVarFirst :: Char -> Bool
isVarFirst c = isLower c || c == '_'

isVarLeft :: Char -> Bool
isVarLeft c  = isAlphaNum c || c `elem` ['_', '\'']

isOpChar :: Char -> Bool
isOpChar c = c `elem` ("+-*/^=<>!" :: String)

lexeme :: Parser a -> Parser a
lexeme p = p <* hspace

nextNotVar :: Parser ()
nextNotVar = notFollowedBy (satisfy isVarLeft)

symbol :: Text -> Parser Text
symbol t = lexeme (try (string t <* nextNotVar)) 

toBool :: Parser Text -> Parser Bool
toBool p = lexeme $ p >>= \case 
        "true"  -> return True
        "false" -> return False
        _       -> fail "expected bool"

parseILit :: Parser E'
parseILit = withSpan $ ILit <$> lexeme (decimal <* nextNotVar <?> "integer literal")

parseBLit :: Parser E'
parseBLit = withSpan $ BLit <$> toBool (symbol "true" <|> symbol "false" <?> "bool")

varName :: Parser Text
varName = cons <$> satisfy isVarFirst <*> takeWhileP Nothing isVarLeft

parseVar' :: Parser Text
parseVar' = lexeme $ do
    name <- lookAhead varName
    if name `elem` reserved then fail $ "reserved symbol: " ++ show name 
    else varName

parseVar :: Parser E'
parseVar = withSpan (try (Var <$> parseVar') <?> "variable")

parseParen :: Parser E'
parseParen = withSpan $ between (lexeme (char '(')) (lexeme (char ')')) parseExpr

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
parseAtom = parseILit <|> parseBLit <|> parseVar <|> parseParen

parseLet :: Parser E'
parseLet = withSpan $ try $ Let 
    <$> (symbol "let" *> parseVar')
    <*> (lexeme (char '=' <?> "=") *> parseExpr)
    <*> (symbol "in" *> parseExpr)

parseIf :: Parser E'
parseIf = withSpan $ try $ If
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

parseExpr :: Parser E'
parseExpr = parseIf <|> parseLet <|> parseLambda <|> parseOpr 0

parseDef :: Parser Decl
parseDef = do
    f <- symbol "def" *> parseVar'
    vs <- many parseVar'
    _ <- lexeme (char '=' <?> "=")
    b <- parseExpr
    return $ Def f vs $ foldr Lambda b vs

parseProg :: Parser [(Int, Statement)]
parseProg = sc *> many 
    (   ((,) . unPos . sourceLine <$> getSourcePos)
    <*> (StmtDef <$> parseDef <|> StmtExpr <$> parseExpr) 
    <*  sc)
    where
        sc :: Parser ()
        sc = skipMany (hspace1 <|> void eol)

run :: Text -> Either Text E'
run t = case runParser (parseExpr <* eof) "" t of
    Left  err -> Left $ pack (errorBundlePretty err)
    Right res -> Right res

runProg :: Text -> Either Text [(Int, Statement)]
runProg t = case runParser (parseProg <* eof) "" t of
    Left err -> Left $ pack $ errorBundlePretty err
    Right r  -> Right r

withSpan :: Parser E' -> Parser E'
withSpan p = do
    start <- getSourcePos
    expr  <- p
    end   <- getSourcePos
    return $ At (Span start end) expr