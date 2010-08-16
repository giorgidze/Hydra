{-# LANGUAGE  ForeignFunctionInterface #-}

module Hydra.Stages.JIT (compile) where

import Hydra.FFI.Sundials.IDA (IDAResFn,IDARootFn)
import Hydra.Data
import qualified Hydra.Stages.ToLLVM  as ToLLVM (compile)
import Hydra.Utils.LLVM

import qualified LLVM.FFI.Core as LLVM
import qualified LLVM.FFI.ExecutionEngine as LLVM

import qualified Foreign as FFI
import Foreign (Ptr,FunPtr)
import Foreign.C.Types

compile :: Experiment -> SymTab -> IO (IO (), FFI.FunPtr IDAResFn, FFI.FunPtr IDARootFn)
compile exper symtab = do
  moduleRef <- ToLLVM.compile exper symtab
  -- LLVM.dumpModule moduleRef
  ee <- case execEngine exper of
          JIT -> createJIT moduleRef
          Interpreter -> createInterpreter moduleRef

  jitAllFunctions ee

  case execEngine exper of
    JIT -> do
      hydra_equation_ptr <- jitFunction ee "hydra_equation"
      hydra_event_equation_ptr <- jitFunction ee "hydra_event_equation"
      return (destroyEE ee, hydra_equation_ptr, hydra_event_equation_ptr)
    Interpreter -> do
      hydra_equation_ptr <- wrap_hydra_equation (hydra_equation ee)
      hydra_event_equation_ptr <- wrap_hydra_event_equation (hydra_event_equation ee)
      return (destroyEE ee, hydra_equation_ptr, hydra_event_equation_ptr)


hydra_equation :: EE -> CDouble -> Ptr a -> Ptr a -> Ptr a -> Ptr a -> IO CInt
hydra_equation ee d p1 p2 p3 p4 = do
  gvd <- LLVM.createGenericValueOfFloat LLVM.doubleType d
  args <- mapM LLVM.createGenericValueOfPointer [p1,p2,p3,p4]
  gvr <- runFunction ee "hydra_equation" (gvd : args)
  let r = fromIntegral (LLVM.genericValueToInt gvr 0)
  mapM_ destroyGV (gvr : gvd : args)
  return r

hydra_event_equation :: EE -> CDouble -> Ptr a -> Ptr a -> Ptr a -> Ptr a -> IO CInt
hydra_event_equation ee d p1 p2 p3 p4 = do
  gvd <- LLVM.createGenericValueOfFloat LLVM.doubleType d
  args <- mapM LLVM.createGenericValueOfPointer [p1,p2,p3,p4]
  gvr <- runFunction ee "hydra_event_equation" (gvd : args)
  let r = fromIntegral (LLVM.genericValueToInt gvr 0)
  mapM_ destroyGV (gvr : gvd : args)
  return r

foreign import ccall safe "wrapper"
  wrap_hydra_equation :: (CDouble -> Ptr a -> Ptr a -> Ptr a -> Ptr a -> IO CInt) -> IO (FunPtr a)

foreign import ccall safe "wrapper"
  wrap_hydra_event_equation :: (CDouble -> Ptr a -> Ptr a -> Ptr a -> Ptr a -> IO CInt) -> IO (FunPtr a)