{-# LANGUAGE QuasiQuotes, GADTs #-}

module Main where

import Hydra
import Hydra.Solver.Sundials

type Flange  = (Double,Double) -- (Angle, flow Torque)
type Engine  = SR Flange
type Wheel   = SR Flange
type Gear    = SR (Flange,Flange)

type State = (Double,Double) -- (Angle, Angular Velocity)

type Machine  = SR Flange

engine1 :: Double -> Engine
engine1 meanTau = [rel| (phi, tau) ->
    tau = $meanTau$
|]

engine2 :: Double -> Engine
engine2 meanTau = [rel| (phi, tau) ->
    local transm
    transm = 1 + sin phi
    tau = transm * $meanTau$
|]

engineSwitch :: Engine
engineSwitch = switch (engine2 10)
                      [fun| (phi,_) -> der phi - 40 |]
                      (\_ -> engine1 10)


wheel :: Double -> State -> Wheel
wheel inertia (phi0,w0) = [rel| (phi,tau) ->
    init phi = $phi0$

    local w
    init w = $w0$

    w = der phi
    tau = der w * $inertia$
|]

gear :: Double -> Gear
gear ratio = [rel| ((phi1,tau1),(phi2,tau2)) ->
    $ratio$ * phi1 = phi2
    - tau1 = $ratio$ * tau2
|]

machine ::  State -> Machine
machine s = [rel| (phi,tau) ->
    local ePhi
    local eTau
    local g1Phi
    local g1Tau
    local g2Phi
    local g2Tau

    $wheel 1.0 s$  <> (phi,-tau)
    $gear 1.8$     <> ((g1Phi,g1Tau),(g2Phi,g2Tau))
    $engineSwitch$ <> (ePhi,eTau)

    ePhi = g1Phi
    eTau + g1Tau = 0

    g2Phi = phi
    g2Tau - tau = 0
|]

mainSR :: SR ()
mainSR = [rel| () ->
    local a1
    local t1
    $ machine (0,0) $ <> (a1,t1)
|]

main :: IO ()
main = simulate experimentDefault{solver = sundials} mainSR