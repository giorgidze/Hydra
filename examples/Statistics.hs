module Main where

import Text.Printf

type SData = [Double]
type Result = ([(Double,Double)],Double)

-- Symbolic Processing CPU time:  1.0769297e-4
-- JIT Compilation CPU time:      1.09993e-2
-- Numerical Simulation CPU time: 4.99967e-2
-- Event Handling CPU time:       6.39958e-6

-- Symbolic Processing CPU time:  3.456442e-5
-- JIT Compilation CPU time:      7.6660999999999995e-3
-- Numerical Simulation CPU time: 7.66616e-2


-- Symbolic Processing CPU time:  6.33292e-2
-- JIT Compilation CPU time:      1.056598
-- Numerical Simulation CPU time: 9.272727999999999
-- Event Handling CPU time:       3.963075e-3

-- Symbolic Processing CPU time:  0.1469904
-- JIT Compilation CPU time:      2.119862
-- Numerical Simulation CPU time: 23.228486
-- Event Handling CPU time:       6.092936e-3

-- Symbolic Processing CPU time:  0.2356513
-- JIT Compilation CPU time:      3.213124
-- Numerical Simulation CPU time: 45.140391
-- Event Handling CPU time:       8.472780000000001e-3

-- Symbolic Processing CPU time:  0.3279786
-- JIT Compilation CPU time:      4.506373
-- Numerical Simulation CPU time: 86.47102699999999
-- Event Handling CPU time:       1.1365927e-2

-- Symbolic Processing CPU time:  0.438638
-- JIT Compilation CPU time:      5.659631
-- Numerical Simulation CPU time: 105.066482
-- Event Handling CPU time:       1.471904e-2

-- Symbolic Processing CPU time:  0.5339653
-- JIT Compilation CPU time:      6.839554
-- Numerical Simulation CPU time: 152.250072



pendulum :: SData
pendulum = [
    1.0769297e-4
  , 1.09993e-2
  , 4.99967e-2
  , 6.39958e-6
  ]

freeFall :: SData
freeFall = [
    3.456442e-5
  , 7.6660999999999995e-3
  , 7.66616e-2
  ]

rlc1 :: SData
rlc1 = [
    6.33292e-2
  , 1.056598
  , 9.272727999999999
  , 3.963075e-3
  ]

rlc2 :: SData
rlc2 = [
    0.1469904
  , 2.119862
  , 23.228486
  , 6.092936e-3
  ]

rlc3 :: SData
rlc3 = [
    0.2356513
  , 3.213124
  , 45.140391
  , 8.472780000000001e-3
  ]

rlc4 :: SData
rlc4 = [
    0.3279786
  , 4.506373
  , 86.47102699999999
  , 1.1365927e-2
  ]

rlc5 :: SData
rlc5 = [
    0.438638
  , 5.659631
  , 105.066482
  , 1.471904e-2
  ]

rlc6 :: SData
rlc6 = [
   0.5339653
 , 6.839554
 , 152.250072
 ]


statistics :: SData -> Result
statistics sdata =
  let s = sum sdata
      p = map (\x -> (x,(x / s) * 100)) sdata
  in  (p,s)

printResult :: Result -> IO ()
printResult ([], d) = do putStrLn "-----"
                         printf "%.5f\n\n\n" d
printResult ((t,p) : tps, d) = do printf "%.5f"   t
                                  printf "  %.2f\n" p
                                  printResult (tps,d)

main :: IO ()
main = do
  let sdatas = [pendulum,freeFall,rlc1,rlc2,rlc3,rlc4,rlc5,rlc6]
  mapM_ printResult $ map statistics sdatas