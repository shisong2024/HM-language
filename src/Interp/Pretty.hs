{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Interp.Pretty where

import Interp.Types
import Data.Text (Text, pack)
import qualified Data.Text as T
import Text.Megaparsec (SourcePos (sourceName))
import qualified Data.Text.IO as TIO
import Data.List (sortOn, find)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Interp.TypeCheck

prettyBOpr :: Opr -> Text
prettyBOpr opr = maybe "?" fst (find ((== opr) . snd) opTable)

debugE' :: E' -> Text
debugE' = \case
    ILit n -> pack (show n)
    BLit b -> pack (show b)
    Var t -> t
    ListLit es -> "[" <> T.intercalate ", " (fmap debugE' es) <> "]"
    TupleLit es -> "(" <> T.intercalate ", " (fmap debugE' es) <> ")"
    BOpr op e1 e2 -> "(" <> T.unwords [debugE' e1, prettyBOpr op, debugE' e2] <> ")"
    Let t e1 e2 -> T.unwords ["(Let", t, "=", debugE' e1, "in", debugE' e2] <> ")"
    If eb e1 e2 -> T.unwords ["(If", debugE' eb, "then", debugE' e1, "else", debugE' e2] <> ")"
    Lambda t e -> T.concat ["(\\", t, " -> ", debugE' e, ")"]
    App e1 e2 -> "(App " <> T.unwords [debugE' e1, debugE' e2] <> ")"
    Match e arms -> T.unwords ["(Match", debugE' e, "with"] <> " " 
        <> T.intercalate " | " [prettyP p <> " -> " <> debugE' b | (p, b) <- arms] <> ")"
    AnnT e _ -> "(" <> debugE' e <> " :: ..."
    At _ e -> debugE' e

prettyV' :: V' -> Text
prettyV' = \case
    VInt n -> pack $ show n
    VBool b -> if b then "true" else "false"
    VList vs -> "[" <> T.intercalate ", " (fmap prettyV' vs) <> "]"
    VTuple vs -> "(" <> T.intercalate ", " (fmap prettyV' vs) <> ")"
    VClosure t _ _ -> T.unwords ["<closure", t, "::", "...>"]
    VPrim n _ args -> primLike n args
    VCtor n _ args -> primLike n args
    where
        primLike :: Text -> [V'] -> Text
        primLike n = \case
            [] -> n
            args -> "(" <> T.unwords (n: map prettyV' args) <> ")"

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
    OprArgIsNotComparable v1 v2 -> T.unwords [prettyV' v1, "and", prettyV' v2, "are not comparable"] <> "."

printT' :: T' -> Text
printT' = go False
    where
        go :: Bool -> T' -> Text
        go needParen = \case
            TInt -> "Int"
            TBool -> "Bool"
            TList tp -> "[" <> go False tp <> "]"
            TTuple tps -> "(" <> T.intercalate ", " (fmap (go False) tps) <> ")"
            TVar i -> "a" <> pack (show i)
            TFunc t1 t2 -> "(" <> T.unwords [go False t1, "->", go False t2] <> ")"
            TCon n x -> case x of
                [] -> n
                args -> parenIf needParen (T.unwords (n: fmap (go True) args))
        
        parenIf :: Bool -> Text -> Text
        parenIf b t = if b then "(" <> t <> ")" else t
                

prettyT' :: T' -> Text
prettyT' = printT' . normalizeT

prettyTypeError :: TypeError -> Text
prettyTypeError = \case
    UnboundVar t -> T.unwords [t, "is an unbound variable."]
    TypeMismatch t1 t2 -> case normalizeTs [t1, t2] of
        [a, b] -> T.unwords ["Type mismatch: expected", printT' b, "but got", printT' a] <> "."
        _ -> "Type mismatch."
    DuplicateDef tx -> "Duplicated definition: " <> tx <> "."
    OccurCheck -> "Occurs check failed."
    DuplicatePatVar x -> "The variable " <> x <> " is bound more than once in the same pattern."
    NonExhaustivePat t -> "Patterns are not exhaustive for type " <> prettyT' t <> "."
    UnknownCtor t -> "Unknown constructor: " <> t <> "."
    CtorArityMisMatch c n m -> 
        T.unwords ["Constructor", c, "expects", pack (show n), "argument(s) but the pattern has", pack (show m)] <> "."
    DuplicateCtor c -> "Duplicate constructor: " <> c <> "."
    DuplicateData d -> "Duplicate data declaration: " <> d <> "."
    UnknownTypeCtor t -> "Unknown type: " <> t <> "."
    TypeCtorArityMisMatch t n m -> 
        T.unwords ["Type", t, "expects", pack (show n), "argument(s) but got", pack (show m)] <> "."
    CalleeNote n sch e ->
        prettyTypeError e <> "\n  note: `" <> n <> "` is bound here with type " <> prettyS' sch <> "."
    WithNote nt e -> prettyTypeError e <> "\n" <> T.unlines (map ("  " <>) (T.lines nt))

prettyS' :: S' -> Text
prettyS' (Forall tvs t)
    | null tvs  = prettyT' t
    | otherwise = 
        let l = S.size tvs - 1
            bs = S.toList tvs
            ap = dedup $ varOrder t
            bsOrd = filter (`elem` bs) ap <> filter (`notElem` ap) bs
            frees = dedup (varOrder (renumberT (M.fromList $ zip bsOrd [0..]) t)) L.\\ [0..l-1]
            ren = M.fromList (zip bsOrd [0..] <> zip frees [l..])
        in "Forall " <> T.unwords ["a" <> pack (show i) | i <- [0..l]] <> ". " <> printT' (renumberT ren t)


prettyP :: P' -> Text
prettyP = \case
    PVar x -> x
    PWild -> "_"
    PInt n -> pack $ show n
    PBool b ->  if b then "true" else "false"
    PNil -> "[]"
    PCons h t -> prettyP h <> " : " <> prettyP t
    PTuple ps -> "(" <> T.intercalate ", " (map prettyP ps) <> ")"
    PCtor c l -> if null l then c else "(" <> T.unwords (c: map prettyP l) <> ")"

prettyEvalErrorWith :: BatchName -> Text -> Located EvalError -> Text
prettyEvalErrorWith = renderLocated prettyEvalError

prettyTypeErrorWith :: BatchName -> Text -> Located TypeError -> Text
prettyTypeErrorWith = renderLocated prettyTypeError

---

lineAt :: Text -> Int -> Text
lineAt src n = case drop (n - 1) (T.lines src) of
    []     -> ""
    (l: _) -> T.stripEnd l

renderLocated :: (a -> Text) -> BatchName -> Text -> Located a -> Text
renderLocated f batchName tx (Located msp err) = case msp of
    Nothing -> f err
    Just sp@(Span n _)
        | sourceName n /= T.unpack batchName -> f err
        | T.null line || col > T.length line + 1 -> f err 
        | otherwise -> T.intercalate "\n" 
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

printBatch :: Text -> BatchName -> [(Int, Statement)] -> M.Map Int StmtTy -> M.Map Int (Either (Located EvalError) V') -> [(Int, Text)] -> IO ()
printBatch src bn lprs tps vals perr = 
    mapM_ (TIO.putStr . snd) . sortOn fst $
        [(ln, renderStmt ln i st) | (i, (ln, st)) <- zip [0..] lprs] <>
        [(i, T.pack (show i) <> ": Parse Error:\n" <> msg <> "\n") | (i, msg) <- perr]

    where
        renderStmt :: Show a => a -> Int -> Statement -> Text
        renderStmt ln i st = T.pack (show ln) <> ": " <> case st of
            StmtDef (Def n _ _) -> case M.lookup i tps of
                Just (TyDef _ sch) -> "def " <> n <> " : " <> prettyS' sch <> "\n"
                _ -> ""
            StmtExpr _ -> case (M.lookup i tps, M.lookup i vals) of
                (Just (TyExpr (Left terr)), _) -> "Type Error:\n" <> prettyTypeErrorWith src bn terr <> "\n"
                (Just (TyExpr (Right tp)), Just (Left eerr)) -> "Type : " <> prettyT' tp <> "\n" <>
                    "Eval Error:\n" <> prettyEvalErrorWith src bn eerr <> "\n"
                (Just (TyExpr (Right tp)), Just (Right v)) -> "Type : " <> prettyT' tp <> "\n" <> "Value: " <> prettyV' v <> "\n"
                _ -> ""
            StmtData d -> "data " <> T.unwords (dName d : dParams d) <> "\n"
                <> T.concat [ "  " <> c <> " : " <> (\(Forall _ t) -> prettyT' t) sch <> "\n"
                | (c, sch) <- M.toAscList (ctorSchemes d) ]

renumberT :: M.Map TypeVar TypeVar -> T' -> T'
renumberT m = go
    where
        go :: T' -> T'
        go = \case
            TInt -> TInt
            TBool -> TBool
            TList t -> TList (go t)
            TTuple ts -> TTuple (fmap go ts)
            TFunc g r -> TFunc (go g) (go r)
            TVar i -> TVar (M.findWithDefault i i m)
            TCon n args -> TCon n (fmap go args)

varOrder :: T' -> [TypeVar]
varOrder = \case
    TInt -> []
    TBool -> [] 
    TVar i -> [i]
    TList t -> varOrder t
    TTuple ts -> concatMap varOrder ts
    TFunc g r -> varOrder g <> varOrder r
    TCon _ args -> concatMap varOrder args

normalizeT :: T' -> T'
normalizeT t = renumberT (M.fromList (zip (dedup (varOrder t)) [0..])) t

normalizeTs :: [T'] -> [T']
normalizeTs ts = fmap (renumberT $ M.fromList (zip (dedup (concatMap varOrder ts)) [0..])) ts

dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x: xs) = x: dedup (filter (/= x) xs)