{-# LANGUAGE QuasiQuotes, GADTs #-}

module Main where

import Hydra
import Hydra.Solver.Sundials

vanDerPol :: Double -> SR ()
vanDerPol lambda = [rel| () ->
    local x y
    init x = 1
    init y = 1
    y = der x
    der y =  - x + $lambda$ * (1 - x * x) * y;
|]

main :: IO ()
main = do
  simulate experimentDefault{solver = sundials} (vanDerPol 3)
