{-# LANGUAGE ConstraintKinds, FlexibleContexts, LambdaCase, OverloadedStrings #-}
module Interp.TypeCheck.Common where

import Interp.Types

import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError (catchError, throwError))

import Data.Text (Text)
import Data.Map (Map, (!?))

import qualified Data.Map as M
import qualified Data.Set as S

type Checker m = (MonadState Counter m, MonadReader DEnv m, MonadError (Located TypeError) m)

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

declName :: Decl -> Text
declName (Def n _ _) = n

unionEff :: Eff -> Eff -> Eff
unionEff a b = EffRow 
    (S.union (effLabs a) (effLabs b))
    (S.union (effVars a) (effVars b))
    (M.unionWith (\x y -> S.toList (S.fromList (x ++ y))) (effParams a) (effParams b))
    (M.fromList [(v, mask v) | v <- S.toList (effVars a `S.union` effVars b), not (S.null (mask v))])
    where
        mask v = S.intersection (get a v) (get b v)
        get x v = if S.member v (effVars x) then M.findWithDefault S.empty v (effExclusions x) else S.empty

removeEff :: Text -> Eff -> Eff
removeEff n e = e 
    { effLabs = S.delete n (effLabs e) 
    , effParams = M.delete n (effParams e)
    , effExclusions = M.fromList 
        [ (v, S.insert n (M.findWithDefault S.empty v (effExclusions e))) 
        | v <- S.toList (effVars e)]
    }

reject :: MonadError (Located TypeError) m => Text -> m a
reject = throwError . Located Nothing . HandleError