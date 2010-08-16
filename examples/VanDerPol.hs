{-# LANGUAGE QuasiQuotes #-}

module Main where

import Hydra

vanDerPol :: Double -> SR ()
vanDerPol lambda = [$sigrel| () where
    local x y
    init x = 1
    init y = 1
    y = der x
    der y = - x + $lambda$ * (1 - x * x) * y;
|]

main :: IO ()
main = do
  simulate defaultExperiment (vanDerPol 3)
