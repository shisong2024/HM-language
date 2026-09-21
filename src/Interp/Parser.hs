{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Interp.Parser where

import Interp.Types
import Text.Megaparsec ((<?>), (<|>), satisfy, MonadParsec (takeWhileP, eof, try), runParser, errorBundlePretty, choice, optional, many, between)
import Text.Megaparsec.Char (space, string, char, space1)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Char (isLower, isAlphaNum)
import Data.Text (cons, Text, pack)
import Data.Functor(($>))
import Control.Monad (guard)

opInfo :: LitOpr -> (Lev, Assoc)
opInfo = \case
    OpAdd -> (1, AssocL)
    OpSub -> (1, AssocL)
    OpMul -> (2, AssocL)
    OpDiv -> (2, AssocL)
    OpPow -> (3, AssocR)

reserved :: [Text]
reserved = ["let", "in", "lambda", "def"]

isVarFirst :: Char -> Bool
isVarFirst c = isLower c || c == '_'

isVarLeft :: Char -> Bool
isVarLeft c  = isAlphaNum c || c `elem` ['_', '\'']

lx :: Parser a -> Parser a
lx p = p <* space

keyword :: Text -> Parser Text
keyword t = try (string t <* space1) <?> ("keyword " ++ show t)

parseLit :: Parser E'
parseLit = Lit <$> lx (decimal <?> "integer literal")

parseVar' :: Parser Text
parseVar' = lx $ do
    name <- cons <$> satisfy isVarFirst <*> takeWhileP Nothing isVarLeft
    if name `elem` reserved then fail $ "reserved keyword: " ++ show name 
    else return name

parseVar :: Parser E'
parseVar = try (Var <$> parseVar') <?> "variable"

parseParen :: Parser E'
parseParen = between (lx (char '(')) (lx (char ')')) parseExpr

parseUnary :: Parser E'
parseUnary = do
    m <- optional $ try $ lx $ char '-'
    case m of
        Nothing -> parseApp
        Just _  -> do
            e <- parseOpr 3
            case e of
                Lit n -> return $ Lit (-n)
                _     -> return $ ArithOpr OpSub (Lit 0) e

parseAtom :: Parser E'
parseAtom = parseLit <|> parseVar <|> parseParen

parseLet :: Parser E'
parseLet = try $ Let 
    <$> (lx (keyword "let") *> parseVar')
    <*> (lx (char '=' <?> "=") *> parseExpr)
    <*> (lx (keyword "in") *> parseExpr)

parseOpr :: Lev -> Parser E'
parseOpr l = parseUnary >>= \t -> lx (loop t)
    where
        loop :: E' -> Parser E'
        loop lhs = do
            mop <- optional $ try $ do
                op <- choice [lx $ string s $> op | (s, op) <- opTable]
                guard (fst (opInfo op) >= l)
                return op
            case mop of
                Nothing -> return lhs
                Just op -> let opt = opInfo op in do
                    let nl = if snd opt == AssocL then fst opt + 1 else fst opt
                    rhs <- parseOpr nl
                    loop (ArithOpr op lhs rhs)

parseLambda :: Parser E'
parseLambda = Lambda <$> (lx (keyword "lambda") *> parseVar') <*> (lx (char '=' <?> "=") *> parseExpr)

parseApp :: Parser E'
parseApp = foldl App <$> parseAtom <*> many parseAtom

parseExpr :: Parser E'
parseExpr = parseLet <|> parseLambda <|> parseOpr 0

run :: Text -> Either Text E'
run t = case runParser (parseExpr <* eof) "" t of
    Left  err -> Left $ pack (errorBundlePretty err)
    Right res -> Right res