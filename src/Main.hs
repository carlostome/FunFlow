module Main (
  analyzeExpr,
  parseExpr,
  printExpr,
  run,
  ) where

import           Expr
import           Infer
import           Parsing
import Type

import           Data.Map                     (Map)
import           Data.Set                     (Set)
import           Text.PrettyPrint.ANSI.Leijen
import           System.IO (stdout)

-- |Parse and label program
parse :: String -> IO (Expr ())
parse programName = do
  let fileName = programName
  content <- readFile fileName
  return (parseExpr content)

run :: String -> IO ()
run name = do
  let p = parseExpr name
  putStrLn "----------------------------------------------"
  putStrLn "-- Original program annotated with PP"      --
  putStrLn "----------------------------------------------"
  putStrLn ""
  let p' = uniquifyPPoints p
  printExpr p'
  case analyzeExpr p' of
    Left err -> do
      putStrLn ""
      printExpr err
    Right (t,_ ,c) -> do
      putStrLn ""
      putStrLn "------------------------------------------"
      putStrLn ""
      print t
      print c

printExpr doc = displayIO stdout (renderPretty 0.4 100 (pretty doc))
              >> putChar '\n'
