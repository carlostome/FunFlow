module Main where

import           Expr
import           Infer
import           Parsing

import           Data.Map                     (Map)
import           Data.Set                     (Set)
import           Text.PrettyPrint.ANSI.Leijen
import           System.IO (stdout)

-- |Parse and label program
parse :: String -> IO (Expr () ())
parse programName = do
  let fileName = programName
  content <- readFile fileName
  return (parseExpr content)

run :: String -> IO ()
run name = do
  p <- parse name
  print "----------------------------------------------"
  print "This is the original program"
  print "----------------------------------------------"
  printExpr p
  case analyzeExpr p of
    Nothing -> print "There was a problem, it must be type incorrect"
    Just (e, t) -> do
      print "------------------------------------------"
      print "This is the annotated program"
      print "------------------------------------------"
      printExpr e
      print t

printExpr doc = displayIO stdout (renderPretty 0.4 100 (pretty doc)) 
              >> putChar '\n'
