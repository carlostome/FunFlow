{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Type
  ( Ty (..)
  , TyScheme(..)
  , Subst
  , empty
  , o
  , Substitutable(..)
  , Constraint(..)
  , Ann(..)
  , TyErr (..)
  )
  where

import           Bound.Class
import           Bound.Scope.Simple
import           Control.Monad      (ap)
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Foldable
import qualified Data.List          as L
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Monoid
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Data.Void
import           Prelude.Extras
import Text.PrettyPrint.ANSI.Leijen as PP hiding (empty)

-- | Ground type
-- Parametrized over type variables and annotation
-- variables.
data Ty b a = TyV a
            | B
            | I
            | Arrow (Ty b a) b (Ty b a)
            | Prod (Ty b a) b (Ty b a)
            | List b (Ty b a)
            deriving (Foldable, Functor, Traversable)

-- | Type scheme.
--   All bound variables appear only at one top-level forall.
data TyScheme b a = Base (Ty b a)
                  | Forall (Scope Int (Ty b) a)
                  deriving (Functor, Foldable, Traversable)

-- | Simple annotations
data Ann a b = AnnV a
             | PP b
             | Union (Ann a b) (Ann a b)
             | Empty
             deriving (Functor, Foldable, Traversable, Eq, Ord)

instance Monoid (Ann a b) where
  mempty  = Empty
  mappend = Union

data Constraint b a = EqC a (Ann b a)
                    deriving (Functor, Foldable, Traversable, Eq, Ord)


data TyErr b a = TyErrOccursCheck a (Ty b a)
               | TyErrUnify       (Ty b a) (Ty b a)
               | TyErrInternal String
               deriving Show

-- | Substitution
-- We maintain the invariant that in a concrete
-- substitution, all annotation variables have
-- been already substituted.
type Subst b a = Map a (Ty b a)

-- | Empty substitution
empty = Map.empty

-- | Compose two substitutions
o :: (Ord a, Ord b) => Subst b a -> Subst b a -> Subst b a
o s1 s2 = Map.map (apply s1) s2 `Map.union` s1

class Substitutable m where
  apply :: (Ord a, Ord b) => Subst b a -> m b a -> m b a

instance Substitutable Ty where
  apply s t = t >>= (\v -> Map.findWithDefault (TyV v) v s)

instance Substitutable TyScheme where
  apply s (Base t)   = Base $ apply s t
  apply s (Forall e) =
    Forall (e >>>= (\v -> Map.findWithDefault (TyV v) v s))

--------------------------------------------------------------------------------
-- Various instances
--------------------------------------------------------------------------------

instance Applicative (Ty b) where
  pure = return
  (<*>) = ap

instance Monad (Ty b) where
  return = TyV
  (TyV a) >>= f = f a
  B   >>= _ = B
  I   >>= _ = I
  (Arrow t1 ann t2) >>= f = Arrow (t1 >>= f) ann (t2 >>= f)

instance (Show b, Show a) => Show (Ty b a) where
  show B  = "Bool"
  show I  = "Integer"
  show (TyV a) = show a
  show (Arrow t1 ann t2) = "(" ++ show t1 ++ " -" ++ show ann
                           ++ "-> " ++ show t2 ++ ")"
  show (Prod t1 ann t2)  = show t1 ++ " x" ++ show ann ++ " " ++ show t2
  show (List ann t) = "[" ++ show t ++ "]" ++ show ann


instance (Show a, Show b) => Show (TyScheme b a) where
  show (Base t)   = show t
  show (Forall s) = "forall " ++ unwords (map show (bindings s))
                    ++ ". " ++ show (fromScope s)

instance (Show a, Show b) => Show (Ann b a) where
  show (AnnV a) = show a
  show (PP p)   = "{" ++ show p ++ "}"
  show (Union a b) = show a ++ " U " ++ show b
  show Empty       = "o"

instance (Show a, Show b) => Show (Constraint b a) where
  show (EqC a ann) = show a ++ "=" ++ show ann

instance (Pretty a, Pretty b) => Pretty (Ty b a) where
  pretty B  = text "Bool"
  pretty I  = text "Integer"
  pretty (TyV a) = pretty a
  pretty (Arrow t1 ann t2) =
    parens (hsep [pretty t1, pretty ann PP.<> text "->"
                 , pretty t2])
  pretty (Prod t1 ann t2)  =
    hsep [pretty t1, char 'x' PP.<> pretty ann, pretty t2]
  pretty (List ann t) =
    brackets (pretty t) PP.<> pretty ann

instance (Pretty a, Pretty b) => Pretty (TyErr b a) where
  pretty err =
    hsep [text "There was an"
            , (bold . red . text $ "error") PP.<> colon]
    PP.<$>
    (indent 2 $
      case err of
        TyErrOccursCheck v t -> hsep [text "Occurs check failed:", pretty v, pretty t]
        TyErrUnify t k -> hsep  [text "Cannot", underline $ text "unify", bold $ pretty t, text "with", bold $ pretty k]
        TyErrInternal s -> hsep [text "Internal error:",  text s])

$(deriveBifunctor ''Ty)
$(deriveBifoldable ''Ty)
$(deriveBitraversable ''Ty)

$(deriveBifunctor ''Ann)
$(deriveBifoldable ''Ann)
$(deriveBitraversable ''Ann)
