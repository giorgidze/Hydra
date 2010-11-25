{-# LANGUAGE ForeignFunctionInterface #-}

module Hydra.Solver.Sundials (sundials) where

import Hydra

import Foreign
import Foreign.C.Types

sundials :: Solver
sundials = Solver {
    createSolver = c_create_solver
  , destroySolver = c_destroy_solver
  , solve = c_solve
  }
  
foreign import ccall safe "create_solver" c_create_solver
  :: CDouble -> CDouble -> Ptr CDouble
  -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt
  -> CInt -> Ptr CInt
  -> Residual -> Residual -> Residual
  -> IO SolverHandle

foreign import ccall safe "destroy_solver" c_destroy_solver
  :: SolverHandle -> IO ()

foreign import ccall safe "solve" c_solve
  :: SolverHandle -> IO CInt