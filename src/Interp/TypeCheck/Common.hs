{-# LANGUAGE ConstraintKinds, FlexibleContexts, LambdaCase, OverloadedStrings #-}
module Interp.TypeCheck.Common where

import Interp.Types

import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError (catchError, throwError))
import Text.Megaparsec (unPos, sourceColumn, sourceLine)
import Data.Text (Text)
import Data.Map (Map, (!?))

type Checker m = (MonadState Counter m, MonadReader DEnv m, MonadError (Located TypeError) m)

errLine :: Located TypeError -> Int
errLine = \case
    Located (Just sp) _ -> fst (spanStart sp)
    _ -> 1

spanStart :: Span -> (Int, Int)
spanStart (Span s _) = (unPos (sourceLine s), unPos (sourceColumn s))

fstDuplicate :: Eq a => [a] -> Maybe a
fstDuplicate = \case
    [] -> Nothing
    x: xs -> if x `elem` xs then Just x else fstDuplicate xs

withNote :: MonadError (Located TypeError) m => Text -> m a -> m a
withNote nt act = catchError act $ \(Located sp err) -> throwError (Located sp $ WithNote nt err)

spinHead :: E' -> Maybe Text
spinHead = \case
    Var n -> Just n
    At _ e -> spinHead e
    App e _ -> spinHead e
    AnnT e _ -> spinHead e
    _ -> Nothing

calleeNote :: MonadError (Located TypeError) m => Map Text S' -> E' -> Located TypeError -> m a
calleeNote env f e@(Located sp err) = case spinHead f of
    Just n | Just sch <- env !? n, informative sch -> throwError $ Located sp $ CalleeNote n sch err
    _ -> throwError e
    where
        bareVar :: T' -> Bool
        bareVar = \case
            TVar _ -> True
            _ -> False

        informative :: S' -> Bool
        informative (Forall tvs _ tp) = not (null tvs) || not (bareVar tp)

swallowedArmsNote :: Text
swallowedArmsNote =
       "This arm's pattern does not fit the scrutinee's type. One common cause:\n"
    <> "if the arm above has an unparenthesised `match` in its body, that inner "
    <> "`match` has absorbed this `|` arm\n"
    <> "-- put parentheses around it to keep the arms where you meant them."

bodySpan :: E' -> Maybe Span
bodySpan = \case
    At sp _ -> Just sp
    _ -> Nothing

tick :: Text -> Text
tick n = "`" <> n <> "`"

declName :: Decl -> Text
declName (Def n _ _) = n