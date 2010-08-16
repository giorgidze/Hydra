{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Hydra.FFI.Sundials.Raw where

import Foreign
import Foreign.C.Types

#include <ida/ida.h>
#include <ida/ida_dense.h>

data NVec

foreign import ccall safe nvCreate   :: CInt -> IO (Ptr NVec)
foreign import ccall safe nvDestroy  :: Ptr NVec -> IO ()
foreign import ccall safe nvLength   :: Ptr NVec -> IO CInt
foreign import ccall safe nvGet      :: Ptr NVec -> CInt -> IO CDouble
foreign import ccall safe nvSet      :: Ptr NVec -> CInt -> CDouble -> IO ()
foreign import ccall safe nvConstant :: Ptr NVec -> CDouble -> IO ()
foreign import ccall safe nvCopy     :: Ptr NVec -> Ptr NVec -> IO ()

foreign import ccall safe "&nvDestroy" nvDestroyPtr :: FunPtr (Ptr NVec -> IO ())

foreign import ccall safe "&nvGet" nvGetPtr :: FunPtr (Ptr NVec -> CInt -> IO CDouble)
foreign import ccall safe "&nvSet" nvSetPtr :: FunPtr (Ptr NVec -> CInt -> CDouble -> IO ())


data IDA
data IDAResFn  -- typedef int (*IDAResFn)(realtype tt, N_Vector yy, N_Vector yp, N_Vector rr, void *user_data);
data IDARootFn -- typedef int (*IDARootFn)(realtype t, N_Vector y, N_Vector yp, realtype *gout, void *user_data);


foreign import ccall safe idaFree :: Ptr IDA -> IO ()

foreign import ccall safe "&idaFree" idaFreePtr :: FunPtr (Ptr IDA -> IO ())

foreign import ccall safe "IDACreate"         idaCreate         :: IO (Ptr IDA)
foreign import ccall safe "IDAInit"           idaInit           :: Ptr IDA -> FunPtr IDAResFn -> CDouble -> Ptr NVec -> Ptr NVec -> IO CInt
foreign import ccall safe "IDASStolerances"   idaSStolerances   :: Ptr IDA -> CDouble -> CDouble -> IO CInt
foreign import ccall safe "IDASetId"          idaSetId          :: Ptr IDA -> Ptr NVec -> IO CInt
foreign import ccall safe "IDASetSuppressAlg" idaSetSuppressAlg :: Ptr IDA -> CInt -> IO CInt
foreign import ccall safe "IDADense"          idaDense          :: Ptr IDA -> CInt -> IO CInt
foreign import ccall safe "IDACalcIC"         idaCalcIC         :: Ptr IDA -> CInt -> CDouble -> IO CInt
foreign import ccall safe "IDARootInit"       idaRootInit       :: Ptr IDA -> CInt -> FunPtr IDARootFn -> IO CInt
foreign import ccall safe "IDASetStopTime"    idaSetStopTime    :: Ptr IDA -> CDouble -> IO CInt
foreign import ccall safe "IDASolve"          idaSolve          :: Ptr IDA -> CDouble -> Ptr CDouble -> Ptr NVec -> Ptr NVec -> CInt -> IO CInt
foreign import ccall safe "IDAGetRootInfo"    idaGetRootInfo    :: Ptr IDA -> Ptr CInt -> IO CInt


-- IDA return flags
ida_SUCCESS, ida_TSTOP_RETURN, ida_ROOT_RETURN :: CInt
ida_SUCCESS      = #const IDA_SUCCESS
ida_TSTOP_RETURN = #const IDA_TSTOP_RETURN
ida_ROOT_RETURN  = #const IDA_ROOT_RETURN

-- itask
ida_NORMAL, ida_ONE_STEP :: CInt
ida_NORMAL   = #const IDA_NORMAL
ida_ONE_STEP = #const IDA_ONE_STEP

-- icopt
ida_YA_YDP_INIT, ida_IDA_Y_INIT :: CInt
ida_YA_YDP_INIT = #const IDA_YA_YDP_INIT
ida_IDA_Y_INIT  = #const IDA_Y_INIT
