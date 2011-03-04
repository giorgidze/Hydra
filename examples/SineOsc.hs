{-# LANGUAGE QuasiQuotes, GADTs #-}

module Main where

import Hydra
import Hydra.Solver.Sundials

sineOsc :: SR ()
sineOsc = [$rel| () ->
  local o
  o = sin time
|]

main :: IO ()
main = simulate experimentDefault{solver = sundials} sineOsc

