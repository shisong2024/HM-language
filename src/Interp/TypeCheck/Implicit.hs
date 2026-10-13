{-# LANGUAGE LambdaCase, FlexibleContexts, OverloadedStrings #-}
module Interp.TypeCheck.Implicit where

import Interp.Types

import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import Control.Monad (unless, when, forM, foldM)

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Set (Set)

import qualified Data.Set as S
import qualified Data.Map as M

import Interp.TypeCheck.Common
import Interp.TypeCheck.Declarations (checkAbilityType)
import Interp.TypeCheck.Solver

isClosedType :: T' -> Bool
isClosedType = S.null . ftv

deferWanted :: Wanted -> Bool
deferWanted w = case wtdName w of
    Just _ -> True
    Nothing -> not (isClosedType (wtdType w))

wantedPred :: Wanted -> Pred
wantedPred w = case wtdName w of
    Nothing -> Implicit $ wtdType w
    Just n  -> NameImplicit n (wtdType w)

resolveWanted :: Checker m => TEnv -> Wanted -> m E'
resolveWanted tenv = resolve []
    where
        resolve path wtd = do
            denv <- ask
            checkAbilityType denv (wtdType wtd)
            let target = wtdType wtd
                failAt e = throwError $ Located (wtdSpan wtd) e
            unless (isClosedType target) $ failAt (AmbiguousImpType target)
            when (target `elem` path) $ failAt (ImpResolutionCycle (reverse (target: path)))
            when (length path >= 64) $ failAt (ImpResolutionDepth (reverse (target: path)))
            selected <- case wtdName wtd of
                Nothing -> search wtd (wtdScope wtd)
                Just n -> byName wtd n (wtdScope wtd)

            let (name, predicates) = selected
            args <- forM predicates $ \p -> resolve (target: path) wtd
                { wtdType = predType p, wtdName = predName p, wtdBind = Nothing }
            return $ foldl App (Var name) args 

        match wtd name = case M.lookup name tenv of
            Nothing -> return Nothing
            Just (Forall vs ps ty) -> do
                fs <- mapM (const fresh) (S.toList vs)
                let renaming = M.fromList (zip (S.toList vs) fs)
                    allowed  = S.unions (map ftv fs)
                return $ case matchCandidateType allowed (apply renaming ty) (wtdType wtd) of
                    Nothing -> Nothing
                    Just su -> Just (name, map (applyPred su . applyPred renaming) ps)

        search wtd = \case
            [] -> throwError $ Located (wtdSpan wtd) (NoImplicit (wtdType wtd))
            frame: outer -> do
                hits <- mapM (match wtd) frame
                case catMaybes hits of
                    []     -> search wtd outer
                    [name] -> return name
                    many  -> throwError $ Located (wtdSpan wtd)
                        (AmbiguousImp (wtdType wtd) (map fst many))

        byName wtd n = \case
            [] -> search wtd (wtdScope wtd)
            frame: outer
                | n `elem` frame -> match wtd n >>= \case
                    Just hit -> return hit
                    _ -> throwError $ Located (wtdSpan wtd) (NoImplicit (wtdType wtd))
                | otherwise -> byName wtd n outer

groupWanted :: [Wanted] -> [(Pred, [Wanted])]
groupWanted = foldl insert []
    where
        insert groups w =
            let p = wantedPred w
            in case break ((== p) . fst) groups of
                (before, []) -> before <> [(p, [w])]
                (before, (q, ws): after) -> before <> ((q, ws <> [w]): after)

validateCandidate :: Checker m => Text -> S' -> m ()
validateCandidate name sch@(Forall _ ps headTy) = do
    denv <- ask
    withNote ("in implicit candidate " <> tick name) (checkAbilityType denv headTy)
    mapM_ (checkAbilityType denv . predType) ps
    let bad reason = throwError $ Located Nothing $ InvalidImpCandidate name reason
        contextVars = S.unions (map (ftv . predType) ps)
    unless (contextVars `S.isSubsetOf` ftv headTy) $
        bad "context contains variables absent from the candidate result type"
    when (any ((== headTy) . predType) ps) $
        bad "context directly requires the candidate's own result type"
    when (sDictCall sch /= emptyEff || sInitEff sch /= emptyEff) $
        bad "automatic dictionary construction must have no immediate effects"
    when (not (null (sEffCs sch)) || not (S.null (typeEffectVars headTy))
        || not (all (S.null . typeEffectVars . predType) ps)) $
        bad "open effect variables in implicit candidates are not supported in R5"

matchCandidateType :: Set TypeVar -> T' -> T' -> Maybe TSub
matchCandidateType allowed= go M.empty
    where
        go su (TVar v) act
            | S.member v allowed = case M.lookup v su of
                Nothing -> Just (M.insert v act su)
                Just old -> if old == act then Just su else Nothing
        go su (TList a) (TList b) = go su a b
        go su (TTuple as) (TTuple bs) = manyTypes su as bs
        go su (TCon a as) (TCon b bs)
            | a == b = manyTypes su as bs
        go su (TFunc a e b) (TFunc x f y)
            | e == f = go su a x >>= \su' -> go su' b y
        go su a b = if a == b then Just su else Nothing

        manyTypes su as bs
            | length as /= length bs = Nothing
            | otherwise = foldM (\acc (a, b) -> go acc a b) su (zip as bs)

candidateHeadsOverlap :: S' -> S' -> Bool
candidateHeadsOverlap (Forall av _ at) (Forall bv _ bt) =
    let rename vs start = M.fromList (zip (S.toList vs) (map TVar [start ..]))
        ar = rename av 1
        br = rename bv (S.size av + 1)
        allowed = S.fromList [1 .. S.size av + S.size bv]
        result = solve allowed M.empty (apply ar at) (apply br bt)
    in case result of Just _ -> True; Nothing -> False
    where
        solve allowed su left right =
            let a = apply su left; b = apply su right
                bind v t = if occursIn v t then Nothing else Just (compose (M.singleton v t) su)
                pairs xs ys = if length xs /= length ys then Nothing
                    else foldM (\acc (x,y) -> solve allowed acc x y) su (zip xs ys)
            in if a == b then Just su else case (a,b) of
                (TVar v,t) | S.member v allowed -> bind v t
                (t,TVar v) | S.member v allowed -> bind v t
                (TList x,TList y) -> solve allowed su x y
                (TTuple xs,TTuple ys) -> pairs xs ys
                (TCon n xs,TCon m ys) | n == m -> pairs xs ys
                (TFunc x e y,TFunc u f v) | e == f -> pairs [x,y] [u,v]
                _ -> Nothing