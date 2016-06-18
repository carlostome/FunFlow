module Main where

import FunFlow
import System.Environment

main :: IO ()
main = do
  [file] <- getArgs
  runFile file
