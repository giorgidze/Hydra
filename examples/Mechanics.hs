{-# LANGUAGE QuasiQuotes #-}

module Main where

import Hydra

type Flange  = (Double,Double) -- (Angle, flow Torque)
type Engine  = SR Flange
type Wheel   = SR Flange
type Gear    = SR (Flange,Flange)

type State = (Double,Double) -- (Angle, Angular Velocity)

type Machine  = SR Flange

engine1 :: Double -> Engine
engine1 meanTau = [$rel| (phi, flow tau) ->
    tau = $meanTau$
|]

engine2 :: Double -> Engine
engine2 meanTau = [$rel| (phi, flow tau) ->
    local transm
    transm = 1 + sin phi
    tau = transm * $meanTau$
|]

wheel :: Double -> Wheel
wheel inertia = [$rel| (phi, flow tau) ->
    local phid
    phid = der phi
    - tau = der phid * $inertia$
|]

gear :: Double -> Gear
gear ratio = [$rel| ((phi1, flow tau1),(phi2, flow tau2)) ->
    $ratio$ * phi1 = phi2
    - tau1 = $ratio$ * tau2
|]

machine ::  State -> Engine -> Machine
machine  (phi0,w0) engine = [$rel| (phi, flow tau) ->
    local phid
    phid = der phi

    init phi = $phi0$
    init phid = $w0$

    reinit phi = cur phi
    reinit phid = cur phid
    
    local ePhi eTau g1Phi g1Tau g2Phi g2Tau

    $wheel 1.0$ <> (phi,tau)
    $gear 1.8$ <> ((g1Phi,g1Tau),(g2Phi,g2Tau))
    $engine$ <> (ePhi,eTau)

    connect      ePhi g1Phi
    connect flow eTau g1Tau

    connect      g2Phi phi
    connect flow g2Tau tau
|]

hybridMachine ::  State -> Machine
hybridMachine s1 = switch (machine s1 (engine2 10))
                          [$fun| (phi,_) -> der phi >= 40 |]  
                          (\s2 -> machine s2 (engine1 10))

mainSR :: SR ()
mainSR = [$rel| () ->
    local a1 t1
    $hybridMachine (0,0) $ <> (a1,t1)
    -- $hybridMachine (pi,0)$ <> (a2,t2)
|]

main :: IO ()
main = do
  -- simulate defaultExperiment{execEngine = Interpreter} mainSR
  simulate defaultExperiment mainSR
  return ()