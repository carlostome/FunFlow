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
data Expr b a
  = Integer Integer
  | Bool    Bool
  | Var     Name
  | Fun     b a  Name Name (Expr b a)
  | Fn      b a  Name (Expr b a)
  | App     (Expr b a) (Expr b a)
  | Let     Name (Expr b a) (Expr b a)
  | ITE     (Expr b a) (Expr b a) (Expr b a)
  | Oper    Op   (Expr b a) (Expr b a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Op
   = Add | Sub | Mul | Div
  deriving (Eq)

bin :: Name -> Expr b a -> Expr b a -> Expr b a
bin op = Oper r where
  r = case op of
        "+" -> Add
        "-" -> Sub
        "*" -> Mul
        "/" -> Div

instance (Show b, Show a) => Pretty (Expr b a) where
  pretty (Integer n) = pretty n
  pretty (Bool b)    = pretty b
  pretty (Var n)     = text n
  pretty (Fun p ann f x e) =
    parens $ hsep [bold (text "fun"), text . show $ p, text . show $ ann, text f, text x
                  ,bold (text "=>"), pretty e]
  pretty (Fn p ann x e) =
    hsep [bold (text "fn"), text . show $ p , text . show $ ann,  text x, bold (text "=>"), pretty e]
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

$(deriveBifunctor ''Expr)
$(deriveBifoldable ''Expr)
$(deriveBitraversable ''Expr)
