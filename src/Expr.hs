{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
module Expr where

import           Data.Bifunctor.TH
import           Text.PrettyPrint.ANSI.Leijen as PP

type Pi    = Int     -- For numbering lambda's etc. that can then be tracked in the analysis
type Name  = String  -- For identifier names

-- | Expr type parametrized over program points
--   and annotations.
data Expr a
  = Integer Integer
  | Bool    Bool
  | Var     Name
  | Fun     a  Name Name (Expr a)
  | Fn      a  Name (Expr a)
  | App     (Expr a) (Expr a)
  | Let     Name (Expr a) (Expr a)
  | ITE     (Expr a) (Expr a) (Expr a)
  | Oper    Op   (Expr a) (Expr a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Op
   = Add | Sub | Mul | Div
  deriving (Eq)

bin :: Name -> Expr a -> Expr a -> Expr a
bin op = Oper r where
  r = case op of
        "+" -> Add
        "-" -> Sub
        "*" -> Mul
        "/" -> Div

instance (Show a) => Pretty (Expr a) where
  pretty (Integer n) = pretty n
  pretty (Bool b)    = pretty b
  pretty (Var n)     = text n
  pretty (Fun p f x e) =
    parens $ hsep [bold (text "fun"), text . show $ p, text f, text x
                  ,bold (text "=>"), pretty e]
  pretty (Fn p x e) =
    hsep [bold (text "fn"), text . show $ p ,  text x, bold (text "=>"), pretty e]
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

instance Show Op where
  show op = case op of
    Add ->  "+"
    Sub ->  "-"
    Mul ->  "*"
    Div ->  "/"

{- $(deriveBifunctor ''Expr) -}
{- $(deriveBifoldable ''Expr) -}
{- $(deriveBitraversable ''Expr) -}
