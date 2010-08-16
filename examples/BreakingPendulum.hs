{-# LANGUAGE QuasiQuotes #-}

module Main where

import Hydra

type Coordinate = (Double,Double)
type Velocity = (Double,Double)
type Body = (Coordinate,Velocity)

g :: Double
g = 9.81

freeFall :: Body -> SR Body
freeFall ((x0,y0),(vx0,vy0)) = [$sigrel| ((x,y),(vx,vy)) where
    init (x,y)  = ($x0$,$y0$)
    init (vx,vy) = ($vx0$,$vy0$)

    reinit (x,y)   = (cur x ,cur y)
    reinit (vx,vy) = (cur vx,cur vy)

    (der x,der y) = (vx,vy)
    (der vx,der vy) = (0.0, - $g$)
|]

pendulum :: Double -> Double -> SR Body
pendulum l phi0 = [$sigrel| ((x,y),(vx,vy)) where
    local phi
    init phi = $phi0$
    init der phi = 0
    init (vx,vy) = (0,0)

    reinit phi = cur phi
    reinit der phi = cur (der phi)
    reinit (vx,vy) = (cur vx,cur vy)

    (x,y) = ($l$ * sin phi, - $l$ * cos phi)
    (vx,vy) = (der x, der y)

    der (der phi) + ($g$ / $l$) * sin phi = 0
|]

breakingPendulum :: Double -> Double -> Double -> SR Body
breakingPendulum t l phi0 = switch (pendulum l phi0) [$sigfun| ((_,_),(_,_)) -> time > $t$ |] freeFall

mainSR :: SR ()
mainSR = [$sigrel| () where
    local x y vx vy
    $breakingPendulum 5 1 (pi / 4)$ <> ((x,y),(vx,vy))
|]

main :: IO ()
main = do
  simulate defaultExperiment mainSR
