{-# LANGUAGE QuasiQuotes #-}

module Main where

import Hydra
import Hydra.Solver.Sundials

functions :: SR ()
functions = [$rel| () ->
    local x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
    local x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
    local x21 x22 x23 x24 x25
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
