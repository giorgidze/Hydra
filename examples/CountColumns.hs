module Main where

import qualified System as System

main :: IO ()
main = do
  args <- System.getArgs
  case args of
    [fn] -> do
      c <- readFile fn
      print $ length $ words $ head $ lines c
    _ -> error "Usage: runhaskell CountColumns.hs plot.dat"
