module FunFlow (
  analyzeExpr,
  runFile,
  runExpr
  ) where

import           Expr
import           Infer
import           Parsing
import           Type

import           Data.Map                     (Map)
import           Data.Set                     (Set)
import           Data.Set                     as S
import           System.IO                    (stdout)
import           Text.PrettyPrint.ANSI.Leijen


-- | Run the analysis on a file
runFile file = parseFile file >>= run

-- | Run the analysis on a string
runExpr      = run . parseExpr

run :: Expr () -> IO ()
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

-- |Parse and label program
parseFile :: String -> IO (Expr ())
parseFile programName = do
  let fileName = programName
  content <- readFile fileName
  return (parseExpr content)
