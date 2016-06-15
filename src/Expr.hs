{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Expr where

import           Data.Bifunctor.TH
import           Text.PrettyPrint.ANSI.Leijen as PP

type Name  = String  -- For identifier names

-- | Expr type parametrized over program points
--   and annotations.
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
  | LCase   r Name Name r Name r
  deriving (Eq, Show, Functor, Foldable, Traversable)

data AnnF p a = AnnF a (ExprF p (AnnF p a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

ann a e = AnnF (Just a) e
noann e = AnnF Nothing e

type Expr p = AnnF p ()

data Op
   = Add | Sub | Mul | Div
  deriving (Eq)

bin :: Name -> Expr p -> Expr p -> Expr p
bin op a b = AnnF () (Oper r a b)
  where
    r = case op of
         "+" -> Add
         "-" -> Sub
         "*" -> Mul
         "/" -> Div

instance (Pretty a, Pretty r) => Pretty (ExprF a r) where
  pretty (Integer n) = pretty n
  pretty (Bool b)    = pretty b
  pretty (Var n)     = text n
  pretty (Fun p f x e) =
    parens $ hsep [bold (text "fun"), pretty p, text f, text x
                  ,bold (text "=>"), pretty e]
  pretty (Fn p x e) =
    hsep [bold (text "fn"), pretty p ,  text x, bold (text "=>"), pretty e]
  pretty (App e1 e2) =
    hsep [pretty e1 , pretty e2]
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
    hsep [bold . text $ "Pair", pretty pi
         , parens (pretty e1 <> comma <> pretty e2)]
  pretty (PCase p x1  x2 e) =
    hsep [bold . text $ "pcase", pretty p, bold . text $ "of"
         ,text "Pair" <> parens (text x1 <> comma <> text x2), bold . text $ "=>"
         ,pretty e]
  pretty (LCons pi e1 e2) =
    hsep [text "Cons", pretty pi , brackets (pretty e1 <> comma <> pretty e2)]
  pretty (LNil pi) = brackets (pretty pi)
  pretty (LCase p x1 x2 e1 x e2) =
    hsep [bold . text $ "lcase", pretty p, bold . text $ "of"
         ,text "Cons" <> parens (text x1 <> comma <> text x2), bold . text $ "=>"
         ,pretty e1, bold . text $ "or", pretty x, bold . text $ "=>", pretty e2]

instance (Pretty p, Pretty a) => Pretty (AnnF a (Maybe p)) where
  pretty (AnnF (Just a) e) = hcat [pretty e, colon, pretty a]
  pretty (AnnF Nothing e)  = pretty e

instance Pretty a => Pretty (AnnF a ()) where
  pretty (AnnF _ e) = pretty e

instance Show Op where
  show op = case op of
    Add ->  "+"
    Sub ->  "-"
    Mul ->  "*"
    Div ->  "/"

$(deriveBifunctor ''ExprF)
$(deriveBifoldable ''ExprF)
$(deriveBitraversable ''ExprF)

$(deriveBifunctor ''AnnF)
$(deriveBifoldable ''AnnF)
$(deriveBitraversable ''AnnF)
