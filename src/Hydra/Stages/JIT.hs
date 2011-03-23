{-# LANGUAGE  ForeignFunctionInterface #-}

module Hydra.Stages.JIT (compile) where

import Hydra.Data
import qualified Hydra.Stages.ToLLVM  as ToLLVM (compile)
import Hydra.Utils.LLVM

import qualified LLVM.FFI.Core as LLVM
import qualified LLVM.FFI.ExecutionEngine as LLVM

import qualified Foreign as FFI
import Foreign (Ptr,FunPtr)
import Foreign.C.Types

compile :: Experiment -> SymTab -> IO (IO (), Residual, Residual, Residual)
compile exper symtab = do
  modRef <- ToLLVM.compile symtab
  -- LLVM.dumpModule modRef
  ee <- if jitCompile exper
           then createJIT modRef
           else createInterpreter modRef

  jitAllFunctions ee

  if jitCompile exper
    then do
      hydra_equation_ptr <- jitFunction ee "hydra_residual_main"
      hydra_event_equation_ptr <- jitFunction ee "hydra_residual_event"
      return (destroyEE ee, FFI.nullFunPtr, hydra_equation_ptr, hydra_event_equation_ptr)
    else do
      hydra_equation_ptr <- wrap_hydra_equation (hydra_equation ee)
      hydra_event_equation_ptr <- wrap_hydra_event_equation (hydra_event_equation ee)
      return (destroyEE ee, FFI.nullFunPtr, hydra_equation_ptr, hydra_event_equation_ptr)


hydra_equation :: EE -> CDouble -> Ptr a -> Ptr a -> Ptr a -> Ptr a -> IO CInt
hydra_equation ee d p1 p2 p3 p4 = do
  gvd <- LLVM.createGenericValueOfFloat LLVM.doubleType d
  args <- mapM LLVM.createGenericValueOfPointer [p1,p2,p3,p4]
  gvr <- runFunction ee "hydra_residual_main" (gvd : args)
  let r = fromIntegral (LLVM.genericValueToInt gvr 0)
  mapM_ destroyGV (gvr : gvd : args)
  return r

hydra_event_equation :: EE -> CDouble -> Ptr a -> Ptr a -> Ptr a -> Ptr a -> IO CInt
hydra_event_equation ee d p1 p2 p3 p4 = do
  gvd <- LLVM.createGenericValueOfFloat LLVM.doubleType d
  args <- mapM LLVM.createGenericValueOfPointer [p1,p2,p3,p4]
  gvr <- runFunction ee "hydra_residual_event" (gvd : args)
  let r = fromIntegral (LLVM.genericValueToInt gvr 0)
  mapM_ destroyGV (gvr : gvd : args)
  return r

foreign import ccall safe "wrapper"
  wrap_hydra_equation :: (CDouble -> Ptr a -> Ptr a -> Ptr a -> Ptr a -> IO CInt) -> IO (FunPtr a)

foreign import ccall safe "wrapper"
  wrap_hydra_event_equation :: (CDouble -> Ptr a -> Ptr a -> Ptr a -> Ptr a -> IO CInt) -> IO (FunPtr a)