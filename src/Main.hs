module Main (
  analyzeExpr,
  parseExpr,
  parseFile,
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
  putStrLn "-- Original program annotated with PP"
  putStrLn ""
  let p' = uniquifyPPoints p
  printDoc p'
  putStrLn ""
  case analyzeExpr p' of
    Left err -> do
      putStrLn ""
      printDoc err
    Right (e_pp, t_pp) -> do
      putStrLn "----------------------------------------------"
      putStrLn "-- Program annotated with CFA analysis"
      putStrLn ""
      printDoc e_pp
      putStrLn ""
      putStr ":: "
      printDoc t_pp
      putStrLn ""

printDoc doc = displayIO stdout (renderPretty 0.4 100 (pretty doc))
              >> putChar '\n'

runFile file = parseFile file >>= run

runExpr = run . parseExpr
