-- (C) 2013-14 Pepijn Kokke & Wout Elsinghorst
-- Modifications made Jurriaan Hage

module Main where

import Expr
import Infer
import Parsing
import Data.Set (Set)
import Data.Map (Map)
import Text.PrettyPrint.ANSI.Leijen

-- |Parse and label program
parse :: String -> IO (Expr () ())
parse programName = do
  let fileName = programName
  content <- readFile fileName
  return (parseExpr content)

run :: String -> IO ()
run name = do
  p <- parse name
  print "--------------------------------------------------------------------------------"
  print "This is the original program"
  print "--------------------------------------------------------------------------------"
  printExpr p
  case analyzeExpr p of
    Nothing -> print "There was a problem, it must be type incorrect"
    Just (e, t) -> do
      print "--------------------------------------------------------------------------------"
      print "This is the annotated program"
      print "--------------------------------------------------------------------------------"
      printExpr e
      print t

