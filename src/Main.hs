module Main (
  analyzeExpr,
  parseExpr,
  printExpr,
  parseFile,
  run,
  runFile,
  runExpr
  ) where

import           Expr
import           Infer
import           Parsing
import Type

import           Data.Map                     (Map)
import           Data.Set                     (Set)
import           Text.PrettyPrint.ANSI.Leijen
import           System.IO (stdout)
import Data.Set as S

-- |Parse and label program
parseFile :: String -> IO (AnnF () ())
parseFile programName = do
  let fileName = programName
  content <- readFile fileName
  return (parseExpr content)

run :: AnnF () () -> IO ()
run p = do
  putStrLn "----------------------------------------------"
  putStrLn "-- Original program annotated with PP       --"
  let p' = uniquifyPPoints p
  printExpr p'
  putStrLn ""
  case analyzeExpr p' of
    Left err -> do
      putStrLn ""
      printExpr err
    Right (ann,an,t,s ,c) -> do
      putStrLn "----------------------------------------------"
      putStrLn "-- Program annotated with types             --"
      printExpr ann
      putStrLn ""
      printExpr an
      print c
      print $ solveConstraints (S.toList c)

printExpr doc = displayIO stdout (renderPretty 0.4 100 (pretty doc))
              >> putChar '\n'

runFile file = parseFile file >>= run
runExpr = run . parseExpr
