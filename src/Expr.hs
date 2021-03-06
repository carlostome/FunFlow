{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Expr where

import           Data.Bifunctor.TH
import           Text.PrettyPrint.ANSI.Leijen as PP
import Data.Bitraversable
import Data.Traversable

type Name  = String  -- For identifier names

-- | Usefull synonym
type Expr p = BiFix ExprF p

-- | Expr type parametrized over program points
-- and recursive construction.
data ExprF a r
  = Integer Integer
  | Bool    Bool
  | Var     Name
  | Fun     a  Name Name r
  | Fn      a  Name r
  | App     r r
  | Let     Name r r
  | ITE     r r r
  | Oper    Op   r r
  | Pair    a r r
  | PCase   r Name Name r
  | LCons   a r r
  | LNil    a
  | LCase   r Name Name r r
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Allowed binary operations
data Op
   = Add | Sub | Mul | Div
  deriving (Eq)

bin :: Name -> Expr a -> Expr a -> Expr a
bin op a b = BiFix (Oper r a b)
  where
    r = case op of
         "+" -> Add
         "-" -> Sub
         "*" -> Mul
         "/" -> Div

newtype BiFix f a = BiFix { unBiFix :: f a (BiFix f a) }

instance Bitraversable f => Functor (BiFix f) where
  fmap = fmapDefault

instance Bitraversable f => Foldable (BiFix f) where
  foldMap = foldMapDefault

instance Bitraversable f => Traversable (BiFix f) where
  traverse f x = fmap BiFix $ bitraverse f (traverse f) $ unBiFix x

--------------------------------------------------------------------------------
-- Various Show and Pretty instances                                          --

instance (Pretty a, Pretty p) => Pretty (ExprF p a) where
  pretty (Integer n) = pretty n
  pretty (Bool b)    = pretty b
  pretty (Var n)     = text n
  pretty (Fun p f x e) =
    parens $ hsep [dullblue (bold (text "fun")), pretty p, text f, text x
                  ,bold (text "=>"), pretty e]
  pretty (Fn p x e) =
    hsep [dullblue (bold (text "fn")), pretty p ,  text x, bold (text "=>"), pretty e]
  pretty (App e1 e2) =
    parens $ hsep [pretty e1 , pretty e2]
  pretty (Let n e e2) =
    hsep [bold $ text "let", text n, bold (char '='), pretty e, bold $ text "in"]
    PP.<$>
    pretty e2
  pretty (ITE c e t) =
    hsep [bold $ text "if", pretty c, bold $ text "then"
         , pretty e, bold $ text "else", pretty t]
  pretty (Oper op e e2) =
    parens $ hsep [pretty e, text (show op), pretty e2]
  pretty (Pair pi e1 e2) =
    hsep [dullgreen . bold . text $ "Pair", pretty pi
         , parens (pretty e1 <> comma <> pretty e2)]
  pretty (PCase p x1  x2 e) =
    vcat [ hsep [bold . text $ "pcase", pretty p, bold . text $ "of"]
         , indent 2 $
            hsep [bold (text "Pair") <> parens (text x1 <> comma <> text x2), bold . text $ "=>"
                 ,pretty e]]
  pretty (LCons pi e1 e2) =
    hsep [dullmagenta . bold . text $ "Cons", pretty pi , parens (pretty e1 <> comma <> pretty e2)]
  pretty (LNil pi) = (dullmagenta . bold . text) "Nil" <+> pretty pi
  pretty (LCase p x1 x2 e1 e2) =
    vcat [ hsep [bold . text $ "lcase", pretty p, bold . text $ "of"]
         , indent 2 $
           vcat [ hsep [bold (text "Cons") <> parens (text x1 <> comma <> text x2), bold . text $ "=>"
                       ,pretty e1]
                , hsep [bold . text $ "or", pretty e2]]]

instance (Pretty (f a (BiFix f a)), Pretty a) => Pretty (BiFix f a) where
  pretty (BiFix f) = pretty f

instance Show Op where
  show op = case op of
    Add ->  "+"
    Sub ->  "-"
    Mul ->  "*"
    Div ->  "/"
$(deriveBifunctor ''ExprF)
$(deriveBifoldable ''ExprF)
$(deriveBitraversable ''ExprF)
