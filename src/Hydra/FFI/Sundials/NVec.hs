module Hydra.FFI.Sundials.NVec (
    NVec
  , nvCreate
  , nvLength
  , nvGet
  , nvSet
  , nvConstant
  , nvCopy
  , nvToList
  , nvToMap
  , nvToUArray
  , nvToForeignPtr
) where

import Foreign
import Data.Array.Unboxed

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Hydra.FFI.Sundials.Raw as FFI

data NVec = NVec !(ForeignPtr FFI.NVec)

nvCreate :: Int -> IO NVec
nvCreate i =
  FFI.nvCreate (fromIntegral i)  >>=
  newForeignPtr FFI.nvDestroyPtr >>=
  return . NVec

nvLength :: NVec -> IO Int
nvLength (NVec fp) =
  withForeignPtr fp  FFI.nvLength >>=
  (return . fromIntegral)

nvGet :: NVec -> Int -> IO Double
nvGet (NVec fp) i =
  withForeignPtr fp (\p -> FFI.nvGet p (fromIntegral i)) >>=
  (return . realToFrac)

nvSet :: NVec -> Int -> Double -> IO ()
nvSet (NVec fp) i d =
  withForeignPtr fp (\p -> FFI.nvSet p (fromIntegral i) (realToFrac d))

nvConstant :: NVec -> Double -> IO ()
nvConstant (NVec fp) d =
  withForeignPtr fp (\p -> FFI.nvConstant p (realToFrac d))

nvCopy :: NVec -> NVec -> IO ()
nvCopy (NVec fp1) (NVec fp2) =
  withForeignPtr fp1 $ \p1 ->
  withForeignPtr fp2 $ \p2 ->
    FFI.nvCopy p1 p2


nvToList :: NVec -> IO [Double]
nvToList v = do
  n <- nvLength v
  xs <- mapM (nvGet v) [0 .. (n - 1)]
  return xs

nvToMap :: NVec -> IO (Map Int Double)
nvToMap v = nvToList v >>= return . Map.fromList . zip [0..] 

nvToUArray :: NVec -> IO (UArray Int Double)
nvToUArray v = do
  n <- nvLength v
  xs <- mapM (nvGet v) [0 .. (n - 1)]
  return (listArray (0,n - 1) xs)

nvToForeignPtr :: NVec -> ForeignPtr FFI.NVec
nvToForeignPtr (NVec fp) = fp