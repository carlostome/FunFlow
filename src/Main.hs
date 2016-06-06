module Main (
  analyzeExpr,
  parseExpr,
  printExpr,
  run,
  run'
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
  p <- parse name
  putStrLn "----------------------------------------------"
  putStrLn "-- Original program annotated with PP"      --
  putStrLn "----------------------------------------------"
  putStrLn ""
  let p' = uniquifyPPoints p
  printExpr p'
  case analyzeExpr p' of
    Nothing -> putStrLn "There was a problem, it must be type incorrect"
    Just (t, c) -> do
      putStrLn ""
      putStrLn "------------------------------------------"
      putStrLn ""
      print t
      print c
      {- print (solveConstraints c :: Map Int (Ann Int Int)) -}
printExpr doc = displayIO stdout (renderPretty 0.4 100 (pretty doc))
              >> putChar '\n'

run' :: String -> IO ()
run' name = do
  let p = parseExpr name
  putStrLn "----------------------------------------------"
  putStrLn "-- Original program annotated with PP"      --
  putStrLn "----------------------------------------------"
  putStrLn ""
  let p' = uniquifyPPoints p
  printExpr p'
  case analyzeExpr p' of
    Nothing -> putStrLn "There was a problem, it must be type incorrect"
    Just (t, c) -> do
      putStrLn ""
      putStrLn "------------------------------------------"
      putStrLn ""
      print t
      print c
