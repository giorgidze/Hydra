module Hydra.Utils.LLVM where

import qualified LLVM.Core as LLVM (initializeNativeTarget)
import qualified LLVM.FFI.Core as LLVM
import qualified LLVM.FFI.ExecutionEngine as LLVM
import qualified LLVM.FFI.BitReader as LLVM

import qualified Foreign as FFI
import qualified Foreign.C.String as FFI

import Control.Monad

data EE = EE {
    eeRefRef :: !(FFI.Ptr (FFI.Ptr LLVM.ExecutionEngine))
  , eeModuleRef :: !LLVM.ModuleRef
  }

createInterpreter :: LLVM.ModuleRef -> IO EE
createInterpreter m = do
  mp <- LLVM.createModuleProviderForExistingModule m
  ee <- FFI.malloc
  _ <- LLVM.createInterpreter ee mp FFI.nullPtr
  return (EE ee m)

createJIT :: LLVM.ModuleRef -> IO EE
createJIT m = do
  LLVM.initializeNativeTarget
  mp <- LLVM.createModuleProviderForExistingModule m
  ee <- FFI.malloc
  _ <- LLVM.createExecutionEngine ee mp FFI.nullPtr
  return (EE ee m)

destroyEE :: EE -> IO ()
destroyEE ee = do
  eeRef <- FFI.peek (eeRefRef ee)
  FFI.newForeignPtr LLVM.ptrDisposeExecutionEngine eeRef >>= FFI.finalizeForeignPtr
  FFI.free (eeRefRef ee)

getFunction :: EE -> String -> IO LLVM.ValueRef
getFunction ee s = FFI.withCString s (LLVM.getNamedFunction (eeModuleRef ee))

jitFunction :: EE -> String -> IO (FFI.FunPtr a)
jitFunction ee s =  do
  f <- getFunction ee s
  eeRef <- FFI.peek (eeRefRef ee)
  LLVM.getPointerToGlobal eeRef f

jitAllFunctions :: EE -> IO ()
jitAllFunctions ee = LLVM.getFirstFunction (eeModuleRef ee) >>= go
  where
  go :: LLVM.ValueRef -> IO ()
  go f | f == FFI.nullPtr = return ()
  go f = do eeRef <- FFI.peek (eeRefRef ee)
            _ <- LLVM.getPointerToGlobal eeRef f
            LLVM.getNextFunction f >>= go

runFunction :: EE -> String -> [LLVM.GenericValueRef] -> IO (LLVM.GenericValueRef)
runFunction ee s args = do
  f <- getFunction ee s
  eeRef <- FFI.peek (eeRefRef ee)
  FFI.withArray args (LLVM.runFunction eeRef f (fromIntegral (length args)))

destroyGV :: LLVM.GenericValueRef -> IO ()
destroyGV gv = FFI.newForeignPtr LLVM.ptrDisposeGenericValue gv >>= FFI.finalizeForeignPtr

-- Adapted from the readBitcodeFromFile function from the llvm package
readBitcode :: FilePath -> IO LLVM.ModuleRef
readBitcode name =
  FFI.withCString name $ \ namePtr ->
  FFI.alloca $ \ bufPtr ->
  FFI.alloca $ \ modPtr ->
  FFI.alloca $ \ errStr -> do
    rrc <- LLVM.createMemoryBufferWithContentsOfFile namePtr bufPtr errStr
    when (rrc /= 0) $ do
      msg <- FFI.peek errStr >>= FFI.peekCString
      fail $ "readBitcode: read return code " ++ show rrc ++ ", " ++ msg

    buf <- FFI.peek bufPtr
    prc <- LLVM.parseBitcode buf modPtr errStr
    when (prc /= 0) $ do
      msg <- FFI.peek errStr >>= FFI.peekCString
      fail $ "readBitcode: parse return code " ++ show prc ++ ", " ++ msg

    ptr <- FFI.peek modPtr
    return ptr