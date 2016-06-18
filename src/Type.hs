{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Type
  ( Ty (..)
  , TyScheme(..)
  , Subst(..)
  , (~>)
  , Substitutable(..)
  , Constraint(..)
  , Ann(..)
  , TyErr (..)
  , (>:)
  )
  where

import           Bound.Class
import           Bound.Scope.Simple
import           Control.Monad                (ap)
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Foldable
import qualified Data.List                    as L
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Monoid
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Void
import           Prelude.Extras
import           Text.PrettyPrint.ANSI.Leijen as PP hiding (empty)

-- | Ground types.
-- Parametrized over type variables and annotation
-- variables.
data Ty b a = TyV a
            | B
            | I
            | TyArrow (Ty b a) b (Ty b a)
            | TyPair  (Ty b a) b (Ty b a)
            | TyList b (Ty b a)
            deriving (Foldable, Functor, Traversable)

instance Applicative (Ty b) where
  pure = return
  (<*>) = ap

instance Monad (Ty b) where
  return = TyV
  (TyV a) >>= f = f a
  B   >>= _ = B
  I   >>= _ = I
  (TyArrow t1 a t2) >>= f = TyArrow (t1 >>= f) a (t2 >>= f)
  (TyPair  t1 a t2) >>= f = TyPair  (t1 >>= f) a (t2 >>= f)
  (TyList a t1) >>= f = TyList a (t1 >>= f)

-- | Type scheme.
--   All bound variables appear only at one top-level forall.
data TyScheme b a = Base (Ty b a)
                  | Forall (Scope Int (Ty b) a)
                  deriving (Functor, Foldable, Traversable)

-- | Simple annotations
data Ann b a = AnnV a
             | AnnS (Set b)
             deriving (Functor, Foldable, Traversable, Eq, Ord)

-- | A constraint
data Constraint b a = C a (Ann b a)
                    deriving (Functor, Foldable, Eq, Ord)

(>:) :: a -> b -> Set (Constraint b a)
a >: b = Set.singleton $ C a (AnnS (Set.singleton b))

-- | Errors that can happen during inference.
data TyErr b a = TyErrOccursCheck a (Ty b a)
               | TyErrUnify       (Ty b a) (Ty b a)
               | TyErrInternal String
               deriving Show

-- | Substitution
newtype Subst b a = Subst (Map a (Ty b a))
  deriving (Show)

(~>) :: Ord a => a -> Ty b a -> Subst b a
v ~> t = Subst $ Map.singleton v t

-- | Compose two substitutions
compose :: Ord a => Subst b a -> Subst b a -> Subst b a
compose s1@(Subst s) (Subst s2) = Subst $ Map.map (apply s1) s2 `Map.union` s

-- | Substitution are Monoids
instance Ord a => Monoid (Subst b a) where
  mappend = compose
  mempty  = Subst Map.empty

-- | A class for types where we can apply
-- substitution.
class Substitutable m where
  apply :: Ord a => Subst b a -> m b a -> m b a

instance Substitutable Ty where
  apply (Subst s) t = t >>= (\v -> Map.findWithDefault (TyV v) v s)

instance Substitutable TyScheme where
  apply s (Base t)   = Base $ apply s t
  apply (Subst s) (Forall e) =
    Forall (e >>>= (\v -> Map.findWithDefault (TyV v) v s))

--------------------------------------------------------------------------------
-- Various Show and Pretty instances
--------------------------------------------------------------------------------

instance (Show b, Show a) => Show (Ty b a) where
  show B  = "bool"
  show I  = "int"
  show (TyV a) = show a
  show (TyArrow t1 ann t2) = "(" ++ show t1 ++ " -" ++ show ann
                           ++ "-> " ++ show t2 ++ ")"
  show (TyPair t1 ann t2)  = show t1 ++ " x" ++ show ann ++ " " ++ show t2
  show (TyList ann t) = "[" ++ show t ++ "]" ++ show ann


instance (Show a, Show b) => Show (TyScheme b a) where
  show (Base t)   = show t
  show (Forall s) = "forall " ++ unwords (map show (bindings s))
                    ++ ". " ++ show (fromScope s)

instance (Show a, Show b) => Show (Ann b a) where
  show (AnnV a) = show a
  show (AnnS p) = "{" ++ show p ++ "}"

instance (Show a, Show b) => Show (Constraint b a) where
  show (C a ann) = show a ++ ">" ++ show ann

instance (Pretty a, Pretty b) => Pretty (Ty b a) where
  pretty B  = text "bool"
  pretty I  = text "int"
  pretty (TyV a) = pretty a
  pretty (TyArrow t1 ann t2) =
    parens (hsep [pretty t1, text "-" PP.<> pretty ann PP.<> text "->"
                 , pretty t2])
  pretty (TyPair t1 ann t2)  =
    parens $ hsep [pretty t1, char 'x' PP.<> pretty ann, pretty t2]
  pretty (TyList ann t) =
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

instance (Pretty a) => Pretty (Set a) where
  pretty = braces . hcat . L.intersperse (char ',') . map pretty . Set.toList

$(deriveBifunctor ''Ty)
$(deriveBifoldable ''Ty)
$(deriveBitraversable ''Ty)
