-- (C) 2013-14 Pepijn Kokke & Wout Elsinghorst
-- Modifications made Jurriaan Hage

module Main where

import Expr
import Parsing
import Data.Set (Set)
import Data.Map (Map)


run :: String -> IO ()
run name = do
  p <- parse name
  putStrLn (show p)
  return ()

-- |Parse and label program
parse :: String -> IO Expr
parse programName = do
  let fileName = programName++".fun"
  content <- readFile fileName
  return (parseExpr content)


