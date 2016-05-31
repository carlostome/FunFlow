{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveAnyClass #-}
module Type
where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Void
import Control.Monad (ap)
import Bound.Scope.Simple
import Bound.Class
import Prelude.Extras
import qualified Data.List as L
import Data.Foldable
import Data.Monoid

-- | Ground type
data Ty a = V a
          | B
          | I
          | Arrow (Ty a) (Ty a)
            deriving (Eq, Show1, Eq1, Foldable, Functor)

instance Show a => Show (Ty a) where
  show B  = "Bool"
  show I  = "Integer"
  show (V a) = show a
  show (Arrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

instance Applicative Ty where
  pure = return
  (<*>) = ap

instance Monad Ty where
  return = V
  (V a) >>= f = f a
  B   >>= _ = B
  I   >>= _ = I
  (Arrow t1 t2) >>= f = Arrow (t1 >>= f) (t2 >>= f)

-- | Type unification
unify :: Ord a => Ty a -> Ty a -> Maybe (Subst Ty a)
unify I I   = Just empty
unify B B   = Just empty
unify (V f1) (V f2)
  | f1 == f2  = Just empty
  | otherwise = Just $ f1 ~> V f2
unify (Arrow t1 t2) (Arrow t1' t2') = do
  s1 <- unify t1 t1'
  s2 <- unify (apply s1 t2) (apply s1 t2')
  return (compose s1 s2)
unify (V f1) t
  | occursCheck f1 t = Nothing
  | otherwise        = Just $ f1 ~> t
unify t (V f1)
  | occursCheck f1 t = Nothing
  | otherwise        = Just $ f1 ~> t
unify _ _ = Nothing

-- | Occurs check
occursCheck :: Eq a => a -> Ty a -> Bool
occursCheck a = getAny . foldMap (Any . (==a))

-- | Type scheme.
-- All bound variables appear only at one top-level forall.
data TyScheme a = Base (Ty a)
                | Forall (Scope Int Ty a)
                deriving (Functor, Foldable, Show1)

{- func :: (Ty a -> Ty b) -> TyScheme a -> TyScheme b -}
{- func f (Base t)  = Base (f t) -}
{- func f (Forall s) = Forall (_ f s) -}

instance (Show a) => Show (TyScheme a) where
  show (Base t)   = show t
  show (Forall s) = "forall " ++ unwords (map show (bindings s))
                    ++ ". " ++ show (fromScope s)

gen' :: Eq a => [a] -> Ty a -> TyScheme a
gen' bv t = Forall $ abstract (`L.elemIndex` (toList t L.\\ bv)) t

inst :: Eq a => [a] -> TyScheme a -> Ty a
inst vs (Forall t) = instantiateVars vs t
inst vs (Base t)   = t

-- | Substitutions
type Subst f a = Map a (f a)

empty = M.empty
(~>) = M.singleton

apply :: Ord a => Subst Ty a -> Ty a -> Ty a
apply subst t = t >>= (\v -> M.findWithDefault (V v) v subst)

apply' :: Ord a => Subst Ty a -> TyScheme a -> TyScheme a
apply' subst (Base t)   = Base $ apply subst t
apply' subst (Forall s) = Forall (s >>>= (\v -> M.findWithDefault (V v) v subst))

compose :: Ord f => Subst Ty f -> Subst Ty f -> Subst Ty f
compose s1 s2 = M.map (apply s1) s2 `M.union` s1

