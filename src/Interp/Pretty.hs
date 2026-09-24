{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Interp.Pretty where

import Interp.Types
import Data.Text (Text, pack)
import qualified Data.Text as T
import Text.Megaparsec (unPos, SourcePos (sourceLine, sourceColumn))
import qualified Data.Text.IO as TIO
import Data.List (sortOn)
import qualified Data.Map as M
import qualified Data.Set as S

prettyBOpr :: Opr -> Text
prettyBOpr = pack . drop 1 . takeWhile (/= 'p') . show

prettyE' :: E' -> Text
prettyE' = \case
    ILit n -> pack (show n)
    BLit b -> pack (show b)
    Var t -> t
    ListLit es -> "[" <> T.intercalate ", " (fmap prettyE' es) <> "]"
    TupleLit es -> "(" <> T.intercalate ", " (fmap prettyE' es) <> ")"
    BOpr op e1 e2 -> "(" <> T.unwords [prettyE' e1, prettyBOpr op, prettyE' e2] <> ")"
    Let t e1 e2 -> T.unwords ["(Let", t, "=", prettyE' e1, "in", prettyE' e2] <> ")"
    If eb e1 e2 -> T.unwords ["(If", prettyE' eb, "then", prettyE' e1, "else", prettyE' e2] <> ")"
    Lambda t e -> T.concat ["(\\", t, " -> ", prettyE' e, ")"]
    App e1 e2 -> "(App " <> T.unwords [prettyE' e1, prettyE' e2] <> ")"
    Match e arms -> T.unwords ["(Match", prettyE' e, "with"] <> " " 
        <> T.intercalate " | " [prettyP p <> " -> " <> prettyE' b | (p, b) <- arms] <> ")"
    At _ e -> prettyE' e

prettyV' :: V' -> Text
prettyV' = \case
    VInt n -> pack $ show n
    VBool b -> if b then "true" else "false"
    VList vs -> "[" <> T.intercalate ", " (fmap prettyV' vs) <> "]"
    VTuple vs -> "(" <> T.intercalate ", " (fmap prettyV' vs) <> ")"
    VClosure t _ _ -> T.unwords ["<closure", t, "::", "...>"]
    VPrim n _ args -> "(" <> T.unwords (n : map prettyV' args) <> ")"

prettyEvalError :: EvalError -> Text
prettyEvalError = \case
    UnboundVariable t -> t <> " is an unbound variable."
    OprArgIsNotNum op v -> T.unwords [prettyBOpr op, "expects a number but gets", prettyV' v] <> "."
    IsNotFunction v -> prettyV' v <> " is not a function."
    DividedByZero _ -> "Divided by zero."
    NegativeExponent i -> "Negative exponent at " <> pack (show i) <> "."
    IfNeedsBool v -> "Expected Bool in if expression but get " <> prettyV' v <> "."
    RecursionLimited n -> "Recursion Limited " <> pack (show n) <> "."
    RecursiveVarDef t -> t <> " is defined recursively as a value."
    NonExhaustiveMatch v -> "Pattern match is not exhaustive: no arm matches " <> prettyV' v <> "."
    ConsNeedsList v -> "cons expects a list as its second argument but gets " <> prettyV' v <> "."

prettyT' :: T' -> Text
prettyT' = \case
    TInt -> "Int"
    TBool -> "Bool"
    TList tp -> "[" <> prettyT' tp <> "]"
    TTuple tps -> "(" <> T.intercalate ", " (fmap prettyT' tps) <> ")"
    TVar i -> "a" <> pack (show i)
    TFunc t1 t2 -> "(" <> T.unwords [prettyT' t1, "->", prettyT' t2] <> ")"

prettyTypeError :: TypeError -> Text
prettyTypeError = \case
    UnboundVar t -> T.unwords [t, "is an unbound variable."]
    TypeMismatch t1 t2 -> T.unwords ["Type mismatch: expected", prettyT' t2, "but got", prettyT' t1] <> "."
    DuplicateDef tx -> "Duplicated definition: " <> tx <> "."
    OccurCheck -> "Occurs check failed."
    DuplicatePatVar x -> "The variable " <> x <> " is bound more than once in the same pattern."
    NonExhaustivePat t -> "Patterns are not exhaustive for type " <> prettyT' t <> "."


prettyS' :: S' -> Text
prettyS' (Forall tvs t)
    | null tvs  = prettyT' t
    | otherwise = "Forall " <> T.unwords ["a" <> pack (show i) | i <- S.toList tvs] <> ". " <> prettyT' t

prettyP :: P' -> Text
prettyP = \case
    PVar x -> x
    PWild -> "_"
    PInt n -> pack $ show n
    PBool b ->  if b then "true" else "false"
    PNil -> "[]"
    PCons h t -> prettyP h <> " : " <> prettyP t
    PTuple ps -> "(" <> T.intercalate ", " (map prettyP ps) <> ")"

lineAt :: Text -> Int -> Text
lineAt src n = case drop (n - 1) (T.lines src) of
    []     -> ""
    (l: _) -> T.stripEnd l

spanStart :: Span -> (Int, Int)
spanStart (Span s _) = (unPos (sourceLine s), unPos (sourceColumn s))

renderLocated :: (a -> Text) -> Text -> Located a -> Text
renderLocated f tx (Located msp err) = case msp of
    Nothing -> f err
    Just sp -> if T.null line
        then f err 
        else T.intercalate "\n" 
            [ numTxt <> " | " <> line
            , gutter <> " | " <> T.replicate (col - 1) " " <> "^"
            , f err
            ]
        where
            ln, col :: Int
            (ln, col) = spanStart sp
            
            line, numTxt, gutter :: Text
            line = lineAt tx ln
            numTxt = T.pack (show ln)
            gutter = T.replicate (T.length numTxt) " "

prettyEvalErrorWith :: Text -> Located EvalError -> Text
prettyEvalErrorWith = renderLocated prettyEvalError

prettyTypeErrorWith :: Text -> Located TypeError -> Text
prettyTypeErrorWith = renderLocated prettyTypeError

printBatch :: Text -> [(Int, Statement)] -> M.Map Int StmtTy -> M.Map Int (Either (Located EvalError) V') -> [(Int, Text)] -> IO ()
printBatch src lprs tps vals perr = 
    mapM_ (TIO.putStr . snd) . sortOn fst $
        [(i, renderStmt i st) | (i, st) <- lprs] <>
        [(i, T.pack (show i) <> ": Parse Error:\n" <> msg <> "\n") | (i, msg) <- perr]

    where
        renderStmt i st = T.pack (show i) <> ": " <> case st of
            StmtDef (Def n _ _) -> case M.lookup i tps of
                Just (TyDef _ sch) -> "def " <> n <> " : " <> prettyS' sch <> "\n"
                _ -> ""
            StmtExpr _ -> case (M.lookup i tps, M.lookup i vals) of
                (Just (TyExpr (Left terr)), _) -> "Type Error:\n" <> prettyTypeErrorWith src terr <> "\n"
                (Just (TyExpr (Right tp)), Just (Left eerr)) -> "Type : " <> prettyT' tp <> "\n" <>
                    "Eval Error:\n" <> prettyEvalErrorWith src eerr <> "\n"
                (Just (TyExpr (Right tp)), Just (Right v)) -> "Type : " <> prettyT' tp <> "\n" <> "Value: " <> prettyV' v <> "\n"
                _ -> ""