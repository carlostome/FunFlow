{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Type
  ( Ty (..)
  , TyScheme(..)
  , unify
  , Subst
  , empty
  , o
  , Substitutable(..)
  )
  where

import           Bound.Class
import           Bound.Scope.Simple
import           Control.Monad      (ap)
import           Data.Foldable
import qualified Data.List          as L
import           Data.Map           (Map)
import qualified Data.Map           as M
import           Data.Monoid
import           Data.Void
import           Prelude.Extras
import           Data.Bifunctor.TH
import Data.Bifunctor

-- | Ground type
-- Parametrized over type variables and annotation
-- variables.
data Ty b a = V a
            | B
            | I
            | Arrow (Ty b a) b (Ty b a)
            deriving (Foldable, Functor, Traversable)

-- | Type scheme.
--   All bound variables appear only at one top-level forall.
data TyScheme b a = Base (Ty b a)
                  | Forall (Scope Int (Ty b) a)
                  deriving (Functor, Foldable, Traversable)

-- | Type unification
unify :: (Ord a, Ord b) => Ty b a -> Ty b a -> Maybe (Subst b a)
unify I I   = Just empty
unify B B   = Just empty
unify (V f1) (V f2)
  | f1 == f2  = Just empty
  | otherwise = Just (M.singleton f1 (V f2), M.empty)
unify (Arrow t1 a t2) (Arrow t1' a' t2') = do
  let phi0 = (M.empty, M.singleton a a')
  phi1 <- unify (apply phi0 t1) (apply phi0 t1')
  phi2 <- unify (apply phi1 . apply phi0 $ t2)
                (apply phi1 . apply phi0 $ t2')
  return (phi2 `o` phi1 `o` phi0)
unify (V f1) t
  | occursCheck f1 t = Nothing
  | otherwise        = Just (M.singleton f1 t, M.empty)
unify t (V f1)
  | occursCheck f1 t = Nothing
  | otherwise        = Just (M.singleton f1 t, M.empty)
unify _ _ = Nothing

-- | Occurs check
occursCheck :: Eq a => a -> Ty b a -> Bool
occursCheck a = getAny . foldMap (Any . (==a))

-- | Substitution
type Subst b a = (Map a (Ty b a), Map b b)

-- | Empty substitution
empty = (M.empty, M.empty)

-- | Compose two substitutions
o :: (Ord a, Ord b) => Subst b a -> Subst b a -> Subst b a
o (s1,s2) (s1', s2') = undefined--(M.map (apply (s1,s2)) s1' `M.union` s1, M.map s2')

class Substitutable m where
  apply :: (Ord a, Ord b) => Subst b a -> m b a -> m b a

instance Substitutable Ty where
  apply (s1,s2) t = first (\x -> M.findWithDefault x x s2) (t >>= (\v -> M.findWithDefault (V v) v s1))

instance Substitutable TyScheme where
  apply s@(s1,s2) (Base t)   = Base $ apply s t
  apply s@(s1,s2) (Forall e) =
    Forall (e >>>= (\v -> M.findWithDefault (V v) v s1))

--------------------------------------------------------------------------------
-- Various instances
--------------------------------------------------------------------------------

instance (Show b, Show a) => Show (Ty b a) where
  show B  = "Bool"
  show I  = "Integer"
  show (V a) = show a
  show (Arrow t1 ann t2) = "(" ++ show t1 ++ " -" ++ show ann
                           ++ "-> " ++ show t2 ++ ")"

instance (Show a, Show b) => Show (TyScheme b a) where
  show (Base t)   = show t
  show (Forall s) = "forall " ++ unwords (map show (bindings s))
                    ++ ". " ++ show (fromScope s)

instance Applicative (Ty b) where
  pure = return
  (<*>) = ap

instance Monad (Ty b) where
  return = V
  (V a) >>= f = f a
  B   >>= _ = B
  I   >>= _ = I
  (Arrow t1 ann t2) >>= f = Arrow (t1 >>= f) ann (t2 >>= f)

$(deriveBifunctor ''Ty)
$(deriveBifoldable ''Ty)
$(deriveBitraversable ''Ty)
