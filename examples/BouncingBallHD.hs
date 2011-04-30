{-# LANGUAGE QuasiQuotes, GADTs #-}

module Main where

import Hydra
import Hydra.Solver.Sundials

type Pos = (Double,Double)
type Vel = (Double,Double)
type Body = (Pos,Vel)

g :: Double
g = 9.81

freeFall :: Body -> SR Body
freeFall ((x0,y0),(vx0,vy0)) = [rel| ((x,y),(vx,vy)) ->
    init ((x,y),(vx,vy)) = (($x0$,$y0$),($vx0$,$vy0$))
    (der x,der y)   = (vx,vy) 
    (der vx,der vy) = (0, - $g$)
|]

bouncingBall :: Body -> SR Body
bouncingBall b = switch (freeFall b) [fun| ((_,y),_) -> y < 0 |] (\(p,v) -> divide (p,v))

divide :: Body -> SR Body
divide ((x0,y0),(vx0,vy0)) = [rel| ((x,y),(vx,vy)) ->
    $bouncingBall ((x0,y0),( vx0 / 2, - vy0 / 2))$ <> ((x,y),(vx,vy))
    local x' y' vx' vy'
    $bouncingBall ((x0,y0),(-vx0 / 2, - vy0 / 2))$ <> ((x',y'),(vx',vy'))
|]

mainSR :: SR ()
mainSR = [rel| () ->
    local x y vx vy
    $bouncingBall ((0,10),(1,0))$ <> ((x,y),(vx,vy))
|]


main :: IO ()
main = simulate experimentDefault{timeStop = 4, solver = sundials} mainSR
