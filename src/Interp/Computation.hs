{-# LANGUAGE LambdaCase #-}
module Interp.Computation where

import Interp.Types

import Data.Maybe (isNothing)
import Data.Text (Text)

import qualified Data.Map as M

bindC :: Computation -> (V' -> Computation) -> Computation
bindC c f = case c of
    Done v -> f v
    Failed e -> Failed e
    Request n o v sp (Continuation k) -> Request n o v sp (Continuation ((`bindC` f) . k))
    Primitive n vs env sp (Continuation k) -> Primitive n vs env sp (Continuation ((`bindC` f) . k))
    FreshResume k -> FreshResume ((`bindC` f) . k)
    UseResume i sp n -> UseResume i sp (bindC n f)

mapFailure :: Span -> Computation -> Computation
mapFailure s = go
    where
        at m = if isNothing m then Just s else m

        go = \case
            Failed (Located sp e) -> Failed (Located (at sp) e)
            Done v -> Done v
            Request n o v sp (Continuation k) -> Request n o v (at sp) (Continuation (go . k))
            Primitive n vs env sp (Continuation k) -> Primitive n vs env (at sp) (Continuation (go . k))
            FreshResume k -> FreshResume (go . k)
            UseResume i sp n -> UseResume i (at sp) (go n)

handleC :: Ord k => (M.Map k V' -> p -> t -> Computation) -> M.Map k V' -> p -> Text -> k -> t -> [(Text, k, k, t)] -> Computation -> Computation
handleC evl env d target ret retB arms = loop
    where
        loop = \case
            Failed e -> Failed e
            Done v -> evl (M.insert ret v env) d retB
            Request n o v sp (Continuation k)
                | n == target -> case [(x, r, b) | (op, x, r, b) <- arms, op == o] of
                    (x, r, b): _ -> FreshResume $ \i ->
                        let resume = VResume i (Continuation (loop . k))
                        in evl (M.insert r resume (M.insert x v env)) d b
                    [] -> Failed (Located sp (UnhandledEff n o))
                | otherwise -> Request n o v sp (Continuation (loop . k))
            Primitive n vs cpd sp (Continuation k) -> Primitive n vs cpd sp (Continuation (loop . k))
            FreshResume k -> FreshResume (loop . k)
            UseResume i sp next -> UseResume i sp (loop next)