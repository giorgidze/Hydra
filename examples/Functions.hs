{-# LANGUAGE QuasiQuotes #-}

module Main where

import Hydra

functions :: SR ()
functions = [$rel| () ->
    local x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25
    x1 = time
    x2  = time + 1
    x3  = time - 1
    x4  = time / 2
    x5  = time * 2
    x6  = time ^ 2
    x7  = logBase (time + 2) (time + 1)
    x8  = - time
    x9  = exp time
    x10 = log (time + 1)
    x11 = sqrt time
    x12 = sin time
    x13 = cos time
    x14 = tan time
    x15 = asin (1 / (time + 1))
    x16 = acos (1 / (time + 1))
    x17 = atan time
    x18 = sinh time
    x19 = cosh time
    x20 = tanh time
    x21 = asinh time
    x22 = acosh (time + 1)
    x23 = atanh (1 / (time + 2))
    x24 = abs (sin time)
    x25 = signum (sin time) 
|]

main :: IO ()
main = do
  simulate defaultExperiment functions
  return ()