{-# LANGUAGE QuasiQuotes, GADTs #-}

module Electronics where

import Hydra
import Hydra.Solver.Sundials

type Pin = (Double,Double)
type Time = Double

twoPin :: SR ((Pin,Pin),Double)
twoPin = [rel| (((p_i,p_v),(n_i,n_v)),u) ->
    p_v - n_v = u
    p_i + n_i = 0
|]

resistor :: Double -> SR (Pin,Pin)
resistor r = [rel| ((p_i, p_v),(n_i, n_v)) ->
    local u
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    $r$ * p_i = u
|]

iInductor :: Double -> Double -> SR (Pin,Pin)
iInductor i0 l = [rel| ((p_i, p_v),(n_i, n_v)) ->
    local u
    init p_i = $i0$
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    $l$ * (der p_i) = u
|]

iCapacitor :: Double -> Double -> SR (Pin,Pin)
iCapacitor u0 c = [rel| ((p_i, p_v),(n_i, n_v)) ->
    local u
    init u = $u0$
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    $c$ * (der u) = p_i
|]

vSourceAC :: Double -> Double -> SR (Pin,Pin)
vSourceAC v f = [rel| ((p_i, p_v), (n_i, n_v)) ->
    local u
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    u = $v$ * sin (2 * $pi$ * $f$ * time)
|]

ground :: SR Pin
ground = [rel| (_,p_v) ->
    p_v = 0
|]

wire :: SR (Pin,Pin)
wire = [rel| ((p_i,p_v),(n_i,n_v)) ->
    local u
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    u = 0
|]

noWire :: SR (Pin,Pin)
noWire = [rel| ((p_i,p_v),(n_i,n_v)) ->
    local u
    $twoPin$ <> (((p_i,p_v),(n_i,n_v)),u)
    p_i = 0
|]

diode :: Bool -> SR (Pin,Pin)
diode False = switch noWire [fun| ((_,p_v),(_,n_v)) -> p_v - n_v > 0 |] (\_ -> diode True)
diode True  = switch wire   [fun| ((p_i,_),(_,_))   -> p_i < 0 |]       (\_ -> diode False)

openedDiode :: SR (Pin,Pin)
openedDiode = diode False

closedDiode :: SR (Pin,Pin)
closedDiode = diode True

serial :: SR (Pin,Pin) -> SR (Pin,Pin) -> SR (Pin,Pin)
serial sr1 sr2 = [rel| ((p_i, p_v),(n_i, n_v)) ->
    local p1_i
    local p1_v
    local n1_i
    local n1_v
    $sr1$  <>  ((p1_i, p1_v), (n1_i, n1_v))
    local p2_i
    local p2_v
    local n2_i
    local n2_v
    $sr2$  <>  ((p2_i, p2_v), (n2_i, n2_v))

    (- p_i) + p1_i = 0
    p_v     = p1_v

    n1_i + p2_i = 0
    n1_v = p2_v

    n2_i + (- n_i) = 0
    n2_v = n_v
|]

parallel :: SR (Pin,Pin) -> SR (Pin,Pin) -> SR (Pin,Pin)
parallel sr1 sr2 = [rel| ((p_i, p_v), (n_i, n_v)) ->
    local p1_i
    local p1_v
    local n1_i
    local n1_v
    $sr1$  <>  ((p1_i, p1_v), (n1_i, n1_v))
    local p2_i
    local p2_v
    local n2_i
    local n2_v
    $sr2$  <>  ((p2_i, p2_v), (n2_i, n2_v))

    (- p_i) + p1_i + p2_i = 0
    p_v  = p1_v
    p1_v = p2_v

    (- n_i) + n1_i + n2_i = 0
    n_v  = n1_v
    n1_v = n2_v
|]

groundedCircuit :: SR (Pin,Pin) -> SR (Pin,Pin) -> SR ()
groundedCircuit sr1 sr2 = [rel| () ->
    local p1_i
    local p1_v
    local n1_i
    local n1_v
    $sr1$  <>  ((p1_i, p1_v), (n1_i, n1_v))

    local p2_i
    local p2_v
    local n2_i
    local n2_v
    $sr2$  <>  ((p2_i, p2_v), (n2_i, n2_v))

    local gp_i
    local gp_v
    $ground$ <> (gp_i,gp_v)

    p1_i + p2_i = 0
    p1_v + p2_v = 0

    n1_i + n2_i + gp_i = 0
    n1_v = n2_v
    n2_v = gp_v
|]

serialise :: [SR (Pin,Pin)] -> SR (Pin,Pin)
serialise = foldr serial wire

parallelise :: [SR (Pin,Pin)] -> SR (Pin,Pin)
parallelise = foldr parallel noWire

-- This is a circuit from our paper
simpleCircuit1 :: SR ()
simpleCircuit1 =
  groundedCircuit (vSourceAC 1 1) (parallel (serial (resistor 1) (iCapacitor 0 1)) (iInductor 0 1))

-- Figure 7.1 in celliers book; Index-0 system, i.e. no algebraic loops and structural singularities
simpleCircuit2 :: SR ()
simpleCircuit2 =
  groundedCircuit (vSourceAC 1 1)
                  (parallel (serial (resistor 1) (iCapacitor 0 1))
                            (serial (resistor 1) (iInductor  0 1))
                  )

halfWaveRectifier :: SR ()
halfWaveRectifier =
  groundedCircuit (vSourceAC 1 1)
                  (serial closedDiode (resistor 1))

-- Figure 9.27 in Cellier's Book
halfWaveRectifierWithCapacitor :: SR ()
halfWaveRectifierWithCapacitor = 
  groundedCircuit (vSourceAC 1 1)
                  (serial (serial   (resistor 1)     closedDiode)
                          (parallel (iCapacitor 0 1) (resistor 1))
                  )

-- Figure 9.31 in Cellier's Book
halfWaveRectifierWithCapacitorAndInductor :: SR ()
halfWaveRectifierWithCapacitorAndInductor =
  groundedCircuit (vSourceAC 1 1)
                  (serialise  [ iInductor 0 1
                              , resistor 1
                              , closedDiode
                              , parallel (iCapacitor 0 1) (resistor 1)
                              ]
                  )

main :: IO ()
main = simulate experimentDefault{solver = sundials} simpleCircuit1


-- Figure 7.5 in celliers book; Index-1 system, i.e. algebraic loop without structural singularities
-- simpleCircuit3 :: SR (Double,Double)
-- simpleCircuit3 = [$hydra|
--   sigrel (i,u) ->
--     init i = 0
--     $resistor  1$   <> ((r1p_i, r1p_v), (r1n_i, r1n_v))
--     $resistor  1$   <> ((r2p_i, r2p_v), (r2n_i, r2n_v))
--     $resistor  1$   <> ((r3p_i, r3p_v), (r3n_i, r3n_v))
--     $inductor  1$   <> ((lp_i, lp_v), (ln_i, ln_v))
--     $vSourceAC 1 1$ <> ((acp_i, acp_v), (acn_i, acn_v))
--     $ground$        <> (gp_i,gp_v)

--     connect acp_i r1p_i lp_i
--     connect acp_v r1p_v lp_v

--     connect r1n_i r2p_i r3p_i
--     connect r1n_v r2p_v r3p_v

--     connect acn_i r3n_i r2n_i ln_i gp_i
--     connect acn_v r3n_v r2n_v ln_v gp_v

--     i = acp_i
--     u = acp_v - acn_v
-- |]

-- -- Figure 7.14 in celliers book; Index-2 system, i.e. algebraic loops and structural singularities
-- -- as excpected DASSLC solver fails here
-- simpleCircuit4 :: SR (Double,Double)
-- simpleCircuit4 = [$hydra|
--   sigrel (i,u) ->
--     init i = 0
--     $resistor  1$   <> ((r1p_i, r1p_v), (r1n_i, r1n_v))
--     $resistor  1$   <> ((r2p_i, r2p_v), (r2n_i, r2n_v))
--     $capacitor 1$   <> ((cp_i, cp_v), (cn_i, cn_v))
--     $inductor  1$   <> ((lp_i, lp_v), (ln_i, ln_v))
--     $vSourceAC 1 1$ <> ((acp_i, acp_v), (acn_i, acn_v))
--     $ground$        <> (gp_i,gp_v)

--     connect acp_i r1p_i cp_i
--     connect acp_v r1p_v cp_v

--     connect r1n_i r2p_i lp_i
--     connect r1n_v r2p_v lp_v

--     connect acn_i ln_i r2n_i cn_i gp_i
--     connect acn_v ln_v r2n_v cn_v gp_v

--     i = acp_i
--     u = acp_v - acn_v
-- |]