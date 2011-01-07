{-# LANGUAGE QuasiQuotes #-}

module Main where

import Hydra
import Hydra.Solver.Sundials

type Body = (Double,Double)

g :: Double
g = 9.81

freeFall :: Body -> SR Body
freeFall (x0,v0) = [rel| (x,v) ->
    init (x,v) = ($x0$,$v0$)
    reinit x = cur x
    reinit v = cur v

    (der x,der v) = (v, - $g$)
|]

bouncingBall :: Body -> SR Body
bouncingBall b = switch (freeFall b) [fun| (x,_) -> x < 0 |] (\(x,v) -> bouncingBall (x,-v))

mainSR :: SR ()
mainSR = [rel| () ->
    local x1 v1
    $bouncingBall (1.0,0.0)$ <> (x1,v1)
    local x2 v2
    $bouncingBall (2.0,0.0)$ <> (x2,v2)
    local x3 v3
    $bouncingBall (3.0,0.0)$ <> (x3,v3)
    local x4 v4
    $bouncingBall (4.0,0.0)$ <> (x4,v4)
|]

main :: IO ()
main = simulate experimentDefault{solver = sundials} mainSR
