-- (C) 2013 Pepijn Kokke & Wout Elsinghorst
-- Adapted by Jurriaan Hage

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Parsing where

import Expr

import Prelude hiding ( abs, sum )
import Data.Char (isSpace)
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.Idioms (iI,Ii (..))
import Text.ParserCombinators.UU.BasicInstances (Parser,pSym)

-- * Top-Level Parsers

parseExpr :: String -> Expr ()
parseExpr = runParser "stdin" pExpr

-- * Parsing the FUN language
pExpr :: Parser (Expr ())
pExpr = (pFn <|> pFun <|> pITE <|> pLet <|> pPair <|> pLCons <|> pPCase
        <|> pLCase) <<|> pBin
  where

  -- literal expressions
  pLit :: Parser (Expr ())
  pLit = (BiFix . Integer) <$> pInteger
     <|> (BiFix . Bool) True  <$ pSymbol "true"
     <|> (BiFix . Bool) False <$ pSymbol "false"
     <|> BiFix (LNil ()) <$ pSymbol "Nil"

  -- atomic expressions
  pAtom = pLit
     <<|> (BiFix . Var) <$> pIdent
     <<|> pParens pExpr

  -- simple expressions
  pFn,pFun,pLet,pITE :: Parser (Expr ())
  pFn     = BiFix <$> iI (Fn ())  "fn" pIdent "=>" pExpr Ii -- Default Pi to 0
  pFun    = BiFix <$> iI (Fun ()) "fun" pIdent pIdent "=>" pExpr Ii -- Dito
  pLet    = BiFix <$> iI Let "let" pIdent "=" pExpr "in" pExpr Ii
  pITE    = BiFix <$> iI ITE "if" pExpr "then" pExpr "else" pExpr Ii

  pPair,pPCase :: Parser (Expr ())
  pPair   = BiFix <$> iI (Pair ()) "Pair" "(" pExpr "," pExpr ")" Ii
  pPCase  = BiFix <$> iI PCase "pcase" pExpr "of" "Pair" "(" pIdent "," pIdent ")" "=>"
               pExpr Ii

  pLCons, pLCase :: Parser (Expr ())
  pLCons   = BiFix <$> iI (LCons ()) "Cons" "(" pExpr "," pExpr ")" Ii
  pLCase   = BiFix <$> iI LCase "lcase" pExpr "of" "Cons" "(" pIdent "," pIdent ")" "=>"
               pExpr "or" pExpr Ii

  -- chained expressions
  pApp = pChainl_ng ((\x y -> BiFix (App x y)) <$ pSpaces) pAtom
  pBin = pChainl_ng (bin <$> pOper) pApp

pIdent,pConst,pOper :: Parser Name
pIdent = lexeme $ (:) <$> pLower <*> pMany (pLetter <|> pDigit <|> pUnderscore)
pConst = lexeme $ (:) <$> pUpper <*> pMany (pLetter <|> pDigit <|> pUnderscore)
pOper  = lexeme $ pSome $ pAnySym "*+/-"

pUnderscore :: Parser Char
pUnderscore = pSym '_'


-- * Recognising more list structures with separators

pFoldr2Sep :: IsParser p => (a -> b -> b, b) -> p a1 -> p a -> p b
pFoldr2Sep alg@(op,e) sep p = must_be_non_empties "pFoldr2Sep" sep p pfm
  where pfm = op <$> p <*> pFoldr1 alg (sep *> p)

pList2Sep :: IsParser p => p a1 -> p a -> p [a]
pList2Sep s p = must_be_non_empties "pListSep" s p (pFoldr2Sep list_alg s p)
