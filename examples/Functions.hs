{-# LANGUAGE QuasiQuotes #-}

module Main where

import Hydra

functions :: SR ()
functions = [$sigrel| () where
    local x x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24
    x = 2 + time ^ 2 / 100
    x1  = time
    x2  = x2
    x3  = x + x - 2 * x
    x4  = x - x
    x5  = x / x
    x6  = x * x / x ^ 2
    x7  = x ^ 2 / x * x
    x8  = logBase x x
    x9  = -     x
    x10 = exp   x
    x11 = log   x
    x12 = sqrt  x
    x13 = sin   x
    x14 = cos   x
    x15 = tan   x
    x16 = asin  (1 / (1 + x))
    x17 = acos  (1 / (1 + x))
    x18 = atan  (1 / (1 + x))
    x19 = sinh  x
    x20 = cosh  x
    x21 = tanh  x
    x22 = asinh x
    x23 = acosh (1 + x)
    x24 = atanh (1 / (1 + x))
    -- x25 = abs (sin time)
    -- x26 = sgn (1 + time)
|]

main :: IO ()
main = do
  simulate defaultExperiment functions
  return ()