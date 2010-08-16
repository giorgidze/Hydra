module Hydra.FFI.Sundials.IDA (
    IDA
  , IDAResFn
  , IDARootFn
  , SolveReturn(..)
  , idaCreate
  , idaInit
  , idaSStolerances
  , idaSetId
  , idaSetSuppressAlg
  , idaDense
  , idaCalcIC
  , idaRootInit
  , idaGetRootInfo
  , idaSetStopTime
  , idaSolve
) where

import Hydra.FFI.Sundials.NVec

import Foreign
import Control.Monad

import Hydra.FFI.Sundials.Raw (IDAResFn,IDARootFn)
import qualified Hydra.FFI.Sundials.Raw as FFI

data IDA = IDA !(ForeignPtr FFI.IDA)

data SolveReturn =
    SolveReturnNormal
  | SolveReturnStop
  | SolveReturnRoot

idaCreate :: IO IDA
idaCreate =
  FFI.idaCreate >>=
  newForeignPtr FFI.idaFreePtr >>=
  return . IDA

idaInit :: IDA -> FunPtr IDAResFn -> Double -> NVec -> NVec -> IO ()
idaInit (IDA idafp) funptr d nv1 nv2 =
  withForeignPtr idafp $ \idap ->
  withForeignPtr (nvToForeignPtr nv1) $ \vp1 -> 
  withForeignPtr (nvToForeignPtr nv2) $ \vp2 -> do
    flag <- FFI.idaInit idap funptr (realToFrac d) vp1 vp2
    when (flag /= FFI.ida_SUCCESS) (fail [])

idaSStolerances :: IDA -> Double -> Double -> IO ()
idaSStolerances (IDA idafp) d1 d2 =
  withForeignPtr idafp $ \idap -> do
    flag <- FFI.idaSStolerances idap (realToFrac d1) (realToFrac d2)
    when (flag /= FFI.ida_SUCCESS) (fail [])

idaSetId :: IDA -> NVec -> IO ()
idaSetId (IDA idafp) nv =
  withForeignPtr idafp $ \idap ->
  withForeignPtr (nvToForeignPtr nv) $ \vp -> do
    flag <- FFI.idaSetId idap vp
    when (flag /= FFI.ida_SUCCESS) (fail [])

idaSetSuppressAlg :: IDA -> Bool -> IO ()
idaSetSuppressAlg (IDA idafp) b = 
  withForeignPtr idafp $ \idap -> do
    flag <- FFI.idaSetSuppressAlg idap (fromIntegral (fromEnum b))
    when (flag /= FFI.ida_SUCCESS) (fail [])

idaDense :: IDA -> Int -> IO ()
idaDense (IDA idafp) i = 
  withForeignPtr idafp $ \idap -> do
    flag <- FFI.idaDense idap (fromIntegral i)
    when (flag /= FFI.ida_SUCCESS) (fail [])

idaCalcIC :: IDA -> Double -> IO ()
idaCalcIC (IDA idafp) d = 
  withForeignPtr idafp $ \idap -> do
    flag <- FFI.idaCalcIC idap FFI.ida_YA_YDP_INIT (realToFrac d)
    when (flag < 0) (fail [])

idaRootInit :: IDA -> Int -> FunPtr IDARootFn -> IO ()
idaRootInit (IDA idafp) i funptr =
  withForeignPtr idafp $ \idap -> do
    flag <- FFI.idaRootInit idap (fromIntegral i) funptr
    when (flag /= FFI.ida_SUCCESS) (fail [])

idaGetRootInfo :: IDA -> Int -> IO [Int]
idaGetRootInfo (IDA idafp) n =
  withForeignPtr idafp $ \idap ->
  allocaArray n $ \rootsp -> do
    flag <- FFI.idaGetRootInfo idap rootsp
    when (flag /= FFI.ida_SUCCESS) (fail [])
    roots <- peekArray n rootsp
    return $ map fst $ filter ( (0 /=) . snd ) $ zip [0 .. ] roots

idaSetStopTime :: IDA -> Double -> IO ()
idaSetStopTime (IDA idafp) d =
  withForeignPtr idafp $ \idap -> do
    flag <- FFI.idaSetStopTime idap (realToFrac d)
    when (flag /= FFI.ida_SUCCESS) (fail [])

idaSolve :: IDA -> Double -> NVec -> NVec -> Bool -> IO (Double,SolveReturn)
idaSolve (IDA idafp) t1 nv1 nv2 b =
  withForeignPtr idafp $ \idap ->
  withForeignPtr (nvToForeignPtr nv1) $ \vp1 ->
  withForeignPtr (nvToForeignPtr nv2) $ \vp2 ->
  alloca $ \tp -> do
    poke tp (realToFrac t1)
    let task = if b then FFI.ida_ONE_STEP else FFI.ida_NORMAL
    flag <- FFI.idaSolve idap (realToFrac t1) tp vp1 vp2 task
    t2 <- peek tp >>= (return . realToFrac)
    case flag of
      _ | flag == FFI.ida_SUCCESS      -> return (t2,SolveReturnNormal)
      _ | flag == FFI.ida_TSTOP_RETURN -> return (t2,SolveReturnStop)
      _ | flag == FFI.ida_ROOT_RETURN  -> return (t2,SolveReturnRoot)
      _ -> fail []