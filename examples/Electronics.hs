{-# LANGUAGE QuasiQuotes #-}

module Electronics where

import Hydra

type Pin = (Double,Double)
type Time = Double

twoPin :: SR (Pin,Pin,Double)
twoPin = [$rel| ((flow p_i,p_v),(flow n_i,n_v),u) ->
    p_v - n_v = u
    p_i + n_i = 0
|]

resistor :: Double -> SR (Pin,Pin)
resistor r = [$rel| ((flow p_i, p_v), (flow n_i, n_v)) ->
    local u
    $twoPin$ <> ((p_i,p_v),(n_i,n_v),u)
    $r$ * p_i = u
|]

inductor :: Double -> SR (Pin,Pin)
inductor l = [$rel| ((flow p_i, p_v), (flow n_i, n_v)) ->
    local u
    reinit p_i = cur p_i
    $twoPin$ <> ((p_i, p_v), (n_i, n_v), u)
    $l$ * (der p_i) = u
|]

iInductor :: Double -> Double -> SR (Pin,Pin)
iInductor i0 l = [$rel| ((flow p_i, p_v), (flow n_i, n_v)) ->
    local u
    init p_i = $i0$
    reinit p_i = cur p_i
    $twoPin$ <> ((p_i, p_v), (n_i, n_v), u)
    $l$ * (der p_i) = u
|]

capacitor :: Double -> SR (Pin,Pin)
capacitor c = [$rel| ((flow p_i, p_v), (flow n_i, n_v)) ->
    local u
    reinit u = cur u
    $twoPin$ <> ((p_i, p_v), (n_i, n_v), u)
    $c$ * (der u) = p_i
|]

iCapacitor :: Double -> Double -> SR (Pin,Pin)
iCapacitor u0 c = [$rel| ((flow p_i, p_v), (flow n_i, n_v)) ->
    local u
    init u = $u0$
    reinit u = cur u
    $twoPin$ <> ((p_i, p_v), (n_i, n_v), u)
    $c$ * (der u) = p_i
|]

vSourceAC :: Double -> Double -> SR (Pin,Pin)
vSourceAC v f = [$rel| ((flow p_i, p_v), (flow n_i, n_v)) ->
    local u
    $twoPin$ <> ((p_i, p_v), (n_i, n_v), u)
    u = $v$ * sin (2 * $pi$ * $f$ * time)
|]

ground :: SR Pin
ground = [$rel| (_,p_v) ->
    p_v = 0
|]

wire :: SR (Pin,Pin)
wire = [$rel| ((flow p_i,p_v),(flow n_i,n_v)) ->
    local u
    $twoPin$ <>((p_i,p_v),(n_i,n_v),u)
    u = 0
|]

noWire :: SR (Pin,Pin)
noWire = [$rel| ((flow p_i,p_v),(flow n_i,n_v)) ->
    local u
    $twoPin$ <>((p_i,p_v),(n_i,n_v),u)
    p_i = 0
|]

diode :: Bool -> SR (Pin,Pin)
diode False = switch noWire [$fun| ((_,p_v),(_,n_v)) -> p_v - n_v > 0 |] (\_ -> diode True)
diode True  = switch wire   [$fun| ((p_i,_),(_,_))   -> p_i < 0 |]       (\_ -> diode False)

openedDiode :: SR (Pin,Pin)
openedDiode = diode False

closedDiode :: SR (Pin,Pin)
closedDiode = diode True

halfWaveRectifier :: SR ()
halfWaveRectifier = [$rel| () ->
    local rp_i  rp_v  rn_i rn_v
    local dp_i  dp_v  dn_i dn_v
    local acp_i acp_v acn_i acn_v
    local g_i   g_v

    $resistor  1$   <> ((rp_i, rp_v), (rn_i, rn_v))
    $closedDiode$   <> ((dp_i, dp_v), (dn_i, dn_v))
    $vSourceAC 1 1$ <> ((acp_i, acp_v), (acn_i, acn_v))
    $ground$        <> (g_i,g_v)

    connect flow acp_i dp_i
    connect      acp_v dp_v

    connect flow dn_i rp_i
    connect      dn_v rp_v

    connect flow acn_i rn_i g_i
    connect      acn_v rn_v g_v
|]

-- Figure 9.27 in Cellier's Book
halfWaveRectifierWithCapacitor :: SR ()
halfWaveRectifierWithCapacitor = [$rel| () ->
    local r1p_i  r1p_v  r1n_i r1n_v
    local r2p_i  r2p_v  r2n_i r2n_v
    local dp_i  dp_v  dn_i dn_v
    local cp_i  cp_v  cn_i cn_v
    local acp_i acp_v acn_i acn_v
    local g_i   g_v

    $resistor  1$    <> ((r1p_i, r1p_v), (r1n_i, r1n_v))
    $resistor  1$    <> ((r2p_i, r2p_v), (r2n_i, r2n_v))
    $closedDiode$    <> ((dp_i, dp_v), (dn_i, dn_v))
    $iCapacitor 0 1$ <> ((cp_i, cp_v), (cn_i, cn_v))
    $vSourceAC 1 1$  <> ((acp_i, acp_v), (acn_i, acn_v))
    $ground$         <> (g_i,g_v)

    connect flow acp_i r1p_i
    connect      acp_v r1p_v

    connect flow r1n_i dp_i
    connect      r1n_v dp_v

    connect flow dn_i cp_i r2p_i
    connect      dn_v cp_v r2p_v

    connect flow acn_i cn_i r2n_i g_i
    connect      acn_v cn_v r2n_v g_v
|]


-- Figure 9.31 in Cellier's Book
halfWaveRectifierWithCapacitorAndInductor :: SR ()
halfWaveRectifierWithCapacitorAndInductor = [$rel| () ->
    local lp_i  lp_v  ln_i  ln_v
    local r1p_i r1p_v r1n_i r1n_v
    local r2p_i r2p_v r2n_i r2n_v
    local dp_i  dp_v  dn_i dn_v
    local cp_i  cp_v  cn_i cn_v
    local acp_i acp_v acn_i acn_v
    local g_i   g_v

    $iInductor 0 1$  <> ((lp_i,  lp_v), (ln_i, ln_v))
    $resistor  1$    <> ((r1p_i, r1p_v), (r1n_i, r1n_v))
    $resistor  1$    <> ((r2p_i, r2p_v), (r2n_i, r2n_v))
    $closedDiode$    <> ((dp_i, dp_v), (dn_i, dn_v))
    $iCapacitor 0 1$ <> ((cp_i, cp_v), (cn_i, cn_v))
    $vSourceAC 1 1$  <> ((acp_i, acp_v), (acn_i, acn_v))
    $ground$         <> (g_i,g_v)

    connect flow acp_i lp_i
    connect      acp_v lp_v

    connect flow ln_i  r1p_i
    connect      ln_v  r1n_v

    connect flow r1n_i dp_i
    connect      r1n_v dp_v

    connect flow dn_i cp_i r2p_i
    connect      dn_v cp_v r2p_v

    connect flow acn_i cn_i r2n_i g_i
    connect      acn_v cn_v r2n_v g_v
|]

simpleCircuit0 :: SR ()
simpleCircuit0 = [$rel| () ->
    local rp_i  rp_v  rn_i  rn_v
    local acp_i acp_v acn_i acn_v
    local g_i   g_v

    $resistor  1$   <> ((rp_i,  rp_v),  (rn_i,  rn_v))
    $vSourceAC 1 1$ <> ((acp_i, acp_v), (acn_i, acn_v))
    $ground$        <> (g_i,g_v)

    connect flow acp_i rp_i
    connect      acp_v rp_v

    connect flow acn_i rn_i g_i
    connect      acn_v rn_v g_v
|]



-- This is a circuit from our paper
-- simpleCircuit1 :: SR (Double,Double)
-- simpleCircuit1 = [$hydra|
--   sigrel (i,u) ->
--     $resistor  1$   <> ((rp_i, rp_v), (rn_i, rn_v))
--     $capacitor 1$   <> ((cp_i, cp_v), (cn_i, cn_v))
--     $inductor  1$   <> ((lp_i, lp_v), (ln_i, ln_v))
--     $vSourceAC 1 1$ <> ((acp_i, acp_v), (acn_i, acn_v))
--     $ground$        <> (gp_i,gp_v)

--     connect flow acp_i rp_i lp_i
--     connect acp_v rp_v lp_v
--     connect flow rn_i cp_i
--     connect rn_v cp_v
--     connect flow acn_i cn_i ln_i gp_i
--     connect acn_v cn_v ln_v gp_v

--     i = acp_i
--     u = acp_v - acn_v
-- |]

-- Figure 7.1 in celliers book; Index-0 system, i.e. no algebraic loops and structural singularities
simpleCircuit2 :: SR ()
simpleCircuit2 = [$rel| () ->
    local r1p_i r1p_v r1n_i r1n_v
    local r2p_i r2p_v r2n_i r2n_v
    local cp_i  cp_v  cn_i  cn_v
    local lp_i  lp_v  ln_i  ln_v
    local acp_i acp_v acn_i acn_v
    local gp_i  gp_v

    init lp_i = 0
    init cp_v - cn_v = 0
    $resistor  1$   <> ((r1p_i, r1p_v), (r1n_i, r1n_v))
    $resistor  1$   <> ((r2p_i, r2p_v), (r2n_i, r2n_v))
    $capacitor 1$   <> ((cp_i, cp_v), (cn_i, cn_v))
    $inductor  1$   <> ((lp_i, lp_v), (ln_i, ln_v))
    $vSourceAC 1 1$ <> ((acp_i, acp_v), (acn_i, acn_v))
    $ground$        <> (gp_i,gp_v)

    connect flow acp_i r1p_i lp_i
    connect acp_v r1p_v lp_v

    connect flow r1n_i r2p_i cp_i
    connect r1n_v r2p_v cp_v

    connect flow acn_i cn_i r2n_i ln_i gp_i
    connect acn_v cn_v r2n_v ln_v gp_v
|]

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

-- serial :: SR (Pin,Pin) -> SR (Pin,Pin) -> SR (Pin,Pin)
-- serial sr1 sr2 = [$rel| ((flow p_i, p_v), (flow n_i, n_v)) ->
--     $sr1$  <>  ((p_i, p_v), (n1_i, n1_v))
--     $sr2$  <>  ((p2_i, p2_v), (n_i, n_v))
--     connect flow n1_i p2_i
--     connect      n1_v p2_v
-- |]

-- parallel :: SR (Pin,Pin) -> SR (Pin,Pin) -> SR (Pin,Pin)
-- parallel sr1 sr2 = [$rel| ((flow p_i, p_v), (flow n_i, n_v)) ->
--     $sr1$  <>  ((p1_i, p1_v), (n1_i, n1_v))
--     $sr2$  <>  ((p2_i, p2_v), (n2_i, n2_v))
--     connect flow p_i p1_i p2_i
--     connect      p_v p1_v p2_v
--     connect flow n_i n1_i n2_i
--     connect      n_v n1_v n2_v
-- |]

-- serialise :: [SR (Pin,Pin)] -> SR (Pin,Pin)
-- serialise = foldl1' serial

-- parallelise :: [SR (Pin,Pin)] -> SR (Pin,Pin)
-- parallelise = foldl1' parallel

-- switchAfter :: Time -> SR (Pin,Pin) -> SR (Pin,Pin) -> SR (Pin,Pin)
-- switchAfter t sr1 sr2 = switch (sr1E) (\_ -> sr2)
--   ->
--   sr1E :: SR((Pin,Pin),E ())
--   sr1E = [$hydra|
--    sigrel (((flow p_i, p_v), (flow n_i, n_v)), event e@()) ->
--      $sr1$ <> ((p_i, p_v),(n_i, n_v))
--      event e = () when time = $t$
--   |]

-- circuitN :: Int -> (Double,Double) -> SR (Double,Double)
-- circuitN n (i1,i2) = [$hydra| 
--   sigrel (i1,i2) ->
--     init i1 = $i1$
--     init i2 = $i2$
--     $resistor  1$   <> ((r1p_i, r1p_v), (r1n_i, r1n_v))

--     $(serialise . map resistor . map fromIntegral) [1 .. n]$   <> ((r2p_i, r2p_v), (r2n_i, r2n_v))

--     $capacitor 1$   <> ((cp_i, cp_v), (cn_i, cn_v))
--     $inductor  1$   <> ((lp_i, lp_v), (ln_i, ln_v))
--     $vSourceAC 1 1$ <> ((acp_i, acp_v), (acn_i, acn_v))
--     $ground$        <> (gp_i,gp_v)

--     connect flow acp_i r1p_i lp_i
--     connect acp_v r1p_v lp_v

--     connect flow r1n_i r2p_i cp_i
--     connect r1n_v r2p_v cp_v

--     connect flow acn_i cn_i r2n_i ln_i gp_i
--     connect acn_v cn_v r2n_v ln_v gp_v

--     i1 = r1p_i
--     i2 = lp_i
-- |]

-- circuitH :: Int -> Double -> (Double,Double) -> SR((Double,Double),E (Double,Double))
-- circuitH n t (i1,i2) = [$hydra|
--    sigrel ((i1,i2), event e@(_,_)) ->
--      $circuitN n (i1,i2)$ <> (i1,i2)
--      event e = (i1,i2) when time = $t$
-- |]


-- circuitR :: SR ()
-- circuitR = [$hydra| 
--   sigrel () ->
--     $resistor 1$  <> ((rp_i, rp_v), (rn_i, rn_v))
--     rp_v = 0
--     rn_v = 0
-- |]


-- circuitS :: SR ()
-- circuitS  = [$hydra| 
--   sigrel () ->
--     init i1 = 0
--     init i2 = 0
--     $resistor 1$   <> ((r1p_i, r1p_v), (r1n_i, r1n_v))

--     $switchAfter 4.3 (resistor 1) noWire$  <> ((r2p_i, r2p_v), (r2n_i, r2n_v))
--     -- $serial (resistor 1) (switchAfter 4.3 wire noWire)$  <> ((r2p_i, r2p_v), (r2n_i, r2n_v))

--     $capacitor 1$   <> ((cp_i, cp_v), (cn_i, cn_v))
--     $inductor  1$   <> ((lp_i, lp_v), (ln_i, ln_v))
--     $vSourceAC 1 1$ <> ((acp_i, acp_v), (acn_i, acn_v))
--     $ground$        <> (gp_i,gp_v)

--     connect flow acp_i r1p_i lp_i
--     connect acp_v r1p_v lp_v

--     connect flow r1n_i r2p_i cp_i
--     connect r1n_v r2p_v cp_v

--     connect flow acn_i cn_i r2n_i ln_i gp_i
--     connect acn_v cn_v r2n_v ln_v gp_v

--     i1 = r1p_i
--     i2 = lp_i
-- |]


-- mainSR :: SR (Double,Double)
-- mainSR =
--     switch        (circuitH  194 10 (0,0))
--   $ \s1 -> switch (circuitH  394 20 s1)
--   $ \s2 -> switch (circuitH  594 30 s2)
--   $ \s3 -> switch (circuitH  794 40 s3)
--   $ \s4 -> switch (circuitH  994 50 s4)
--   $ \s5 ->         circuitN 1194 s5

main :: IO ()
main = do
  simulate defaultExperiment halfWaveRectifierWithCapacitorAndInductor
  return ()