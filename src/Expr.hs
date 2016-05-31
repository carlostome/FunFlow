{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Expr where

import           Text.PrettyPrint.ANSI.Leijen as PP
import System.IO (stdout)


data Op
   = Add | Sub | Mul | Div
  deriving (Eq)

instance Show Op where
  show op = case op of
    Add ->  "+"
    Sub ->  "-"
    Mul ->  "*"
    Div ->  "/"


type Pi    = Integer -- For numbering lambda's etc. that can then be tracked in the analysis
type Name  = String  -- For identifier names

data Expr
  = Integer Integer
  | Bool    Bool
  | Var     Name
  | Fun     Pi   Name Name Expr
  | Fn      Pi   Name Expr
  | App     Expr Expr
  | Let     Name Expr Expr
  | ITE     Expr Expr Expr
  | Oper    Op   Expr Expr
  deriving (Eq,Show)

bin :: Name -> Expr -> Expr -> Expr
bin op x y = Oper r x y where
  r = case op of
        "+" -> Add
        "-" -> Sub
        "*" -> Mul
        "/" -> Div

instance Pretty Expr where
  pretty (Integer n) = pretty n
  pretty (Bool b)    = pretty b
  pretty (Var n)     = text n
  pretty (Fun p f x e) =
    parens $ hsep [bold (text "fun"), pretty p , text f, text x
                  ,bold (text "=>"), pretty e]
  pretty (Fn p x e) =
    hsep [bold (text "fn"), pretty p , text x, bold (text "=>"), pretty e]
  pretty (App e1 e2) =
    hsep [pretty e1 , pretty e2]
  pretty (Let n e e2) =
    hsep [text "let", text n, bold (char '='), pretty e]
    PP.<$>
    nest 2 (hsep [text "in", pretty e2])
  pretty (ITE c e t) =
    hsep [bold $ text "if", pretty c, bold $ text "then"
         , pretty e, bold $ text "else", pretty t]
  pretty (Oper op e e2) =
    parens $ hsep [pretty e, text (show op), pretty e2]

printExpr doc  = displayIO stdout (renderPretty 0.4 100 (pretty doc)) >> putChar '\n'

