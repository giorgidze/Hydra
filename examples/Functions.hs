{-# LANGUAGE QuasiQuotes, GADTs #-}

module Main where

import Hydra
import Hydra.Solver.Sundials

functions :: SR ()
functions = [rel| () ->
    local x1; local x2; local x3; local x4; local x5; local x6; local x7; local x8; local x9; local x10;
    local x11; local x12; local x13; local x14; local x15; local x16; local x17; local x18; local x19; local x20;
    local x21; local x22; local x23; local x24; local x25;
    x1  = 1 / (time + 1)
    x2  = (1 / (time + 1)) + 1
    x3  = (1 / (time + 1)) - 1
    x4  = (1 / (time + 1)) / 2
    x5  = (1 / (time + 1)) * 2
    x6  = (1 / (time + 1)) ^ 2
    x7  = logBase (time + 2) (time + 1)
    x8  = - (1 / (time + 1))
    x9  = exp (1 / (time + 1))
    x10 = log (1 / (time + 1))
    x11 = sqrt (1 / (time + 1))
    x12 = sin (1 / (time + 1))
    x13 = cos (1 / (time + 1))
    x14 = tan (1 / (time + 1))
    x15 = asin (1 / (time + 1))
    x16 = acos (1 / (time + 1))
    x17 = atan (1 / (time + 1))
    x18 = sinh (1 / (time + 1))
    x19 = cosh (1 / (time + 1))
    x20 = tanh (1 / (time + 1))
    x21 = asinh (1 / (time + 1))
    x22 = acosh (time + 1)
    x23 = atanh (1 / (time + 2))
    x24 = abs (sin time)
    x25 = signum (time + 1) 
|]

main :: IO ()
main = simulate experimentDefault{solver = sundials} functions
