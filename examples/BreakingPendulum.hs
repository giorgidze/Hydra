{-# LANGUAGE QuasiQuotes #-}

module Main where

import Hydra
import Hydra.Solver.Sundials

type Coordinate = (Double,Double)
type Velocity = (Double,Double)
type Body = (Coordinate,Velocity)

g :: Double
g = 9.81

freeFall :: Body -> SR Body
freeFall ((x0,y0),(vx0,vy0)) = [$rel| ((x,y),(vx,vy)) ->
    init (x,y)  = ($x0$,$y0$)
    init (vx,vy) = ($vx0$,$vy0$)

    (der x,der y) = (vx,vy)
    (der vx,der vy) = (0.0, - $g$)
|]

pendulum :: Double -> Double -> SR Body
pendulum l phi0 = [$rel| ((x,y),(vx,vy)) ->
    local phi phid

    init phi  = $phi0$
    init phid = 0
    init (x,y) = ($l$ * sin $phi0$, - $l$ * cos $phi0$)

    phid = der phi

    (x,y) = ($l$ * sin phi, - $l$ * cos phi)
    (vx,vy) = (der x, der y)

    der phid + ($g$ / $l$) * sin phi = 0
|]

breakingPendulum :: Double -> Double -> Double -> SR Body
breakingPendulum t l phi0 = switch (pendulum l phi0) [$fun| ((_,_),(_,_)) -> time > $t$ |] freeFall

mainSR :: SR ()
mainSR = [$rel| () ->
    local x y vx vy
    $breakingPendulum 5 1 (pi / 4)$ <> ((x,y),(vx,vy))
|]

main :: IO ()
main = simulate experimentDefault{solver = sundials} mainSR
