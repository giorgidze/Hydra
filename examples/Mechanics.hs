{-# LANGUAGE QuasiQuotes #-}

module Main where

import Hydra

type Flange  = (Double,Double) -- (Angle, flow Torque)
type Engine  = SR Flange
type Wheel   = SR Flange
type Gear    = SR (Flange,Flange)

type State = (Double,Double) -- (Angle, Angular Velocity)

type Machine  = SR Flange 
type MachineE = SR (Flange, E State)

engine1 :: Double -> Engine
engine1 meanTau = [$hydra|
  sigrel (phi, flow tau) where
    tau = $meanTau$
|]

engine2 :: Double -> Engine
engine2 meanTau = [$hydra|
  sigrel (phi, flow tau) where
    transm = 1 + sin phi
    tau = transm * $meanTau$
|]

wheel :: Double -> Wheel
wheel inertia = [$hydra|
  sigrel (phi, flow tau) where
    - tau = der (der phi) * $inertia$
|]

gear :: Double -> Gear
gear ratio = [$hydra|
  sigrel ((phi1, flow tau1),(phi2, flow tau2)) where
    $ratio$ * phi1 = phi2
    - tau1 = $ratio$ * tau2
|]

machine ::  State -> Engine -> Machine
machine  (phi0,w0) engine = [$hydra|
  sigrel (phi, flow tau) where
    init phi = $phi0$
    init der phi = $w0$

    reinit phi = pre phi
    reinit der phi = pre (der phi)

    $wheel 1.0$ <> (phi,tau)
    $gear 1.8$ <> ((g1Phi,g1Tau),(g2Phi,g2Tau))
    $engine$ <> (ePhi,eTau)

    connect      ePhi g1Phi
    connect flow eTau g1Tau

    connect      g2Phi phi
    connect flow g2Tau tau
|]

machineE :: Machine -> MachineE
machineE mach = [$hydra|
  sigrel ((phi, flow tau), event e@(_,_)) where
    $mach$ <> (phi,tau)
    event e = (phi, der phi) when der phi = 40.0
|]

hybridMachine ::  State -> Machine
hybridMachine s1 = switch (machineE $ machine s1 (engine2 10)) (\s2 -> machine s2 (engine1 10))

mainSR :: SR ()
mainSR = [$hydra|
  sigrel () where
    $hybridMachine (0,0) $ <> (a1,t1)
    $hybridMachine (pi,0)$ <> (a2,t2)
|]

main :: IO ()
main = do
  simulate defaultExperiment{execEngine = Interpreter} mainSR
  simulate defaultExperiment mainSR
  return ()