{-# LANGUAGE FlexibleContexts, OverloadedStrings, LambdaCase #-}
module Interp.TypeCheck.Elaboration where

import Interp.Types
import Interp.Eval (stripAt, patVarsList)

import Control.Monad.State (MonadState)
import Control.Monad.Reader (ask)
import Control.Monad (forM_)

import Data.Text (Text, pack)
import Data.Functor ((<&>))
import Data.Set (Set)

import qualified Data.Map as M
import qualified Data.Set as S

import Interp.TypeCheck.Solver (freshHole, ftv, fresh, apply)
import Interp.TypeCheck.Common
import Interp.TypeCheck.Declarations (checkAbilityType)

pureInfer :: E' -> T' -> InferRes
pureInfer e t = InferRes M.empty [] t e emptyEff

underSpan :: Span -> Wanted -> Wanted
underSpan sp w = case wtdSpan w of
    Nothing -> w { wtdSpan = Just sp }
    Just _  -> w

mapInferExpr :: (E' -> E') -> InferRes -> InferRes
mapInferExpr f r = r { irExpr = f (irExpr r) }

freshInternal :: MonadState Counter m => Text -> m Text
freshInternal p = freshHole <&> (("$" <> p <> "@") <>) . pack . show

replaceSolvedHoles :: HoleSol -> E' -> E'
replaceSolvedHoles solved e = let go = replaceSolvedHoles solved in case e of
    ImpHole h -> M.findWithDefault e h solved
    ILit _ -> e
    BLit _ -> e
    SLit _ -> e
    Var _ -> e
    ListLit es -> ListLit (map go es)
    TupleLit es -> TupleLit (map go es)
    Let n a b -> Let n (go a) (go b)
    DictCall h as ds -> DictCall (go h) (map go as) [d { daExpr = go (daExpr d) } | d <- ds]
    If c a b -> If (go c) (go a) (go b)
    BOpr op a b -> BOpr op (go a) (go b)
    Lambda n a -> Lambda n (go a)
    App f x -> App (go f) (go x)
    Match ex arms -> Match (go ex) [(p, go b) | (p, b) <- arms]
    AnnT ex t -> AnnT (go ex) t
    At sp ex -> At sp (go ex)

fillHoles :: HoleSol -> E' -> Either HoleId E'
fillHoles solved = go
    where
        go = \case
            ImpHole h -> maybe (Left h) Right (M.lookup h solved)
            ILit n -> Right (ILit n)
            BLit b -> Right (BLit b)
            SLit s -> Right (SLit s)
            Var n -> Right (Var n)
            ListLit es -> ListLit <$> mapM go es
            TupleLit es -> TupleLit <$> mapM go es
            Let n a b -> Let n <$> go a <*> go b
            DictCall h as ds -> DictCall <$> go h <*> mapM go as
                <*> mapM (\d -> go (daExpr d) >>= \e -> return d {daExpr = e}) ds
            If c a b -> If <$> go c <*> go a <*> go b
            BOpr op a b -> BOpr op <$> go a <*> go b
            Lambda n b -> Lambda n <$> go b
            App f x -> App <$> go f <*> go x
            Match e arms -> Match <$> go e
                <*> mapM (\(p, b) -> (,) p <$> go b) arms
            AnnT e t -> AnnT <$> go e <*> pure t
            At sp e -> At sp <$> go e

prepareDecl :: Checker m => Decl -> m Decl
prepareDecl (Def n ps body) = do
    denv <- ask
    forM_ [(x, t) | ImplicitParam x t <- ps] $ \(x, t) ->
        withNote ("in implicit parameter " <> tick x) (checkAbilityType denv t)
    let vars = S.toList $ S.filter (< 0) $ S.unions
            ([ftv t | ImplicitParam _ t <- ps] <> annotationVars body)
    freshVars <- mapM (const fresh) vars
    let su = M.fromList (zip vars freshVars)
        param = \case
            ImplicitParam x t -> ImplicitParam x (apply su t)
            x -> x
    return $ Def n (map param ps) (mapAnnotations (apply su) False body)

annotationVars :: E' -> [Set TypeVar]
annotationVars = \case
    AnnT e t -> ftv t: annotationVars e
    Let _ _ body -> annotationVars body
    e -> concatMap annotationVars (exprChildren e)

exprChildren :: E' -> [E']
exprChildren = \case
    ListLit es -> es
    TupleLit es -> es
    Let _ a b -> [a, b]
    If a b c -> [a, b, c]
    BOpr _ a b -> [a, b]
    Lambda _ b -> [b]
    App a b -> [a, b]
    Match a arms -> a: map snd arms
    DictCall a args ds -> a: args <> map daExpr ds
    AnnT a _ -> [a]
    At _ a -> [a]
    _ -> []

mapAnnotations :: (T' -> T') -> Bool -> E' -> E'
mapAnnotations f enterLet = go
    where
        go = \case
            AnnT a t -> AnnT (go a) (f t)
            Let n a b -> Let n (if enterLet then go a else a) (go b)
            ListLit es -> ListLit (map go es)
            TupleLit es -> TupleLit (map go es)
            If a b c -> If (go a) (go b) (go c)
            BOpr op a b -> BOpr op (go a) (go b)
            Lambda n a -> Lambda n (go a)
            App a b -> App (go a) (go b)
            Match a arms -> Match (go a) [(pat, go b) | (pat, b) <- arms]
            DictCall a args ds -> DictCall (go a) (map go args)
                [d { daExpr = go (daExpr d) } | d <- ds]
            At sp a -> At sp (go a)
            a -> a

dictHeadName :: E' -> Maybe Text
dictHeadName = \case
    Var n -> Just n
    At _ e -> dictHeadName e
    AnnT e _ -> dictHeadName e
    _ -> Nothing

dictCallHints :: Set Text -> E' -> [(Text, Text)]
dictCallHints bd = go
    where
        go = \case
            DictCall h as ds ->
                let (headE, _) = callSpine h
                    own = case stripAt headE of
                        Var n | not (S.member n bd) -> [(n, daName d) | d <- ds]
                        _ -> []
                in own <> concatMap go (h: as <> map daExpr ds)
            Let n rhs body ->
                let recBd = case stripAt rhs of
                        Lambda _ _ -> S.insert n bd
                        _ -> bd
                in dictCallHints recBd rhs <> dictCallHints (S.insert n bd) body
            Lambda n body -> dictCallHints (S.insert n bd) body
            Match x arms -> go x <> concat
                [dictCallHints (S.union bd (S.fromList (patVarsList pat))) body | (pat, body) <- arms]
            ListLit es -> concatMap go es
            TupleLit es -> concatMap go es
            If c a b -> concatMap go [c, a, b]
            BOpr _ a b -> go a <> go b
            App f a -> go f <> go a
            AnnT x _ -> go x
            At _ x -> go x
            _ -> []

callSpine :: E' -> (E', [E'])
callSpine e = case stripAt e of
    App f a -> let (h, as) = callSpine f in (h, as <> [a])
    _ -> (e, [])
