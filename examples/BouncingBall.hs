{-# LANGUAGE QuasiQuotes #-}

-- module Main where

-- import Hydra

-- g :: Double
-- g = 9.81

-- bouncingBall :: Double -> Double -> SR ()
-- bouncingBall x0 v0 _ = local $ \(x,v) -> 
--   (initial (x ~=~ csig x0)) .
--   (reinit  (x ~=~ pre x)) .
  
--   (initial (v ~=~ csig v0)) .
--   (reinit  (v ~=~ -0.95 * pre v)) .

--   (der x ~=~ v) .
--   (der v ~=~ csig (-g)) .

--   rswitch (cross x 0) id id

-- -- bouncingBall :: Body -> SR Body
-- -- bouncingBall b = switch (ball b) bouncingBall

-- -- mainSR :: SR ()
-- -- mainSR = [$hydra|
-- --   sigrel () where
-- --     monitor x1 using $label "coordinate"$
-- --     $bouncingBall (1.0,0.0)$ <> (x1,v1)
-- --     $bouncingBall (2.0,0.0)$ <> (x2,v2)
-- --     -- $bouncingBall (3.0,0.0)$ <> (x3,v3)
-- --     -- $bouncingBall (4.0,0.0)$ <> (x4,v4)
-- -- |]

-- main :: IO ()
-- main = do
-- --   simulate defaultExperiment{execEngine = Interpreter} mainSR
--   _ <- simulate defaultExperiment (bouncingBall 1 0)
--   return ()

module Main where

import Hydra

type Body = (Double,Double)

g :: Double
g = 9.81

freeFall :: Body -> SR Body
freeFall (x0,v0) = [$sigrel| (x,v) where
    init (x,v) = ($x0$,$v0$)
    reinit x = cur x
    reinit v = cur v

    (der x,der v) = (v, - $g$)
|]

-- ball :: (Body) -> SR (Body, E Body)
-- ball (x0,v0) = [$sigrel| ((x,v),event e@(_,_)) where
--     $freeFall (x0,v0)$ <> (x,v)
--     event e = (0,-v) when x = 0.0
-- |]

bouncingBall :: Body -> SR Body
bouncingBall b = switch (freeFall b) [$sigfun| (x,_) -> x <= 0 |] (\(x,v) -> bouncingBall (x,-v))

mainSR :: SR ()
mainSR = [$sigrel| () where
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
main = do
  simulate defaultExperiment mainSR
