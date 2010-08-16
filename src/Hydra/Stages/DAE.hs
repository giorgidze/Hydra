module Hydra.Stages.DAE (createSolver) where

import Hydra.FFI.Sundials.NVec
import Hydra.FFI.Sundials.IDA

import Foreign
import Control.Monad
import Data.Array.Unboxed

type Solver = Double -> IO (Double, UArray Int Double, Maybe [Int])

createSolver ::
     FunPtr IDAResFn -> Double -> Double -> NVec -> NVec -> NVec
  -> Int -> FunPtr IDARootFn
  -> IO Solver
createSolver resfunptr tStart tStop y yp idv en rootfunptr = do
  neq <- nvLength y
  ida <- idaCreate
  idaInit ida resfunptr tStart y yp
  let reltol = 1.0E-8;
  let abstol = 1.0E-8;
  idaSStolerances ida reltol abstol
  idaSetId ida idv
  idaSetSuppressAlg ida False
  idaDense ida neq
  idaCalcIC ida (tStart + 0.00001)
  when (en > 0) (idaRootInit ida en rootfunptr)
  idaSetStopTime ida tStop

  let solver :: Solver
      solver t1 = do
        (t2,r) <- idaSolve ida t1 y yp False
        y2 <- nvToUArray y
        case r of
          SolveReturnNormal -> return (t2,y2,Nothing)
          SolveReturnStop   -> return (t2,y2,Just [])
          SolveReturnRoot   -> do evs <- idaGetRootInfo ida en
                                  return (t2,y2,Just evs)

  return solver