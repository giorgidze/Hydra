{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface #-}

module Hydra.Stages.Simulate (simulate) where

import Hydra.FFI.Sundials.NVec

import Hydra.Data
import Hydra.Stages.BuildNewToOld
import Hydra.Stages.HandleEvents
import Hydra.Stages.BuildEvents
import Hydra.Stages.Flatten
import Hydra.Stages.Validate
import qualified Hydra.Stages.JIT as JIT (compile)
import qualified Hydra.Stages.DAE as DAE

import qualified Data.Array.Unboxed as Array
import Data.Array.Unboxed (UArray)
import qualified Data.Map as Map
import System.IO (hFlush,stdout)

simulate :: Experiment -> SR () -> IO ()
simulate exper sr = case sr of
  SigRel f1 -> simulateAux exper (empty{model = f1 (Unit ())})
  Switch sr1 _ _ -> simulate exper sr1

simulateAux :: Experiment -> SymTab -> IO ()
simulateAux exper symtab = do
  let symtab1 = validate $ flatten
                         $ buildEvents
                         $ handleEvents
                         $ buildNewToOld
                         $ symtab

  let nVar     = variableNumber     symtab1
  let nVarEv   = eventNumber        symtab1

  (cleanup, equation_ptr,event_equation_ptr) <- JIT.compile exper symtab1

  y   <- nvCreate nVar
  nvConstant y 0.0
  yp  <- nvCreate nVar
  nvConstant yp 0.0
  idv <- nvCreate nVar
  nvConstant idv 0.0

  initialiseVectors symtab1 y idv
  
  zeroCrossingHack y
  solver <- DAE.createSolver equation_ptr (timeStart exper) (timeStop exper) y yp idv nVarEv event_equation_ptr

  let loop :: Bool -> Double -> IO ()
      loop firstCall t1 = do
        (t2,y2,r) <- solver t1
        case r of
          Nothing -> do
            printSolution symtab1 t2 y2
            loop False (t2 + timeStep exper)
          Just [] -> do
            printSolution symtab1 t2 y2
          Just _ | firstCall -> do
            printSolution symtab1 t2 y2
            loop False (t2 + timeStep exper)
          Just evs1 -> do -- zeroCrossingHack y
                          -- zeroCrossingHack yp
                          instants1     <- nvToMap y
                          instantsDiff1 <- nvToMap yp
                          cleanup
                          let f :: (Int,Bool) -> (Int,Bool)
                              f (i,_) = if elem i evs1 then (i,True) else (i,False)
                          let symtabNew = symtab1 {instants = instants1, instantsDiff = instantsDiff1, events = Map.map f (events symtab1)}
                          simulateAux (exper{timeStart = t2}) (symtabNew{timeCurrent = t2})

  nvToUArray y >>= printSolution symtab1 (timeStart exper)
  loop True (timeStart exper + timeStep exper)

initialiseVectors :: SymTab -> NVec -> NVec -> IO ()
initialiseVectors symtab y idv = mapM_ go (Map.assocs (variables symtab))
  where
  go :: (Int,Maybe Double) -> IO ()
  go (_,Nothing)  = return ()
  go (i,Just d)   = do nvSet y i d
                       nvSet idv i 1.0

printSolution :: SymTab -> Double -> UArray Int Double -> IO ()
printSolution symtab t v = do
  putStr (show t)
  let is = if (Map.null (monitors symtab))
              then (let b = Array.bounds v in [fst b .. snd b])
              else Map.keys (monitors symtab)
  mapM_ (\i -> putStr (' ' : show (v  Array.! i))) is
  putStrLn []
  hFlush stdout

zeroCrossingHack :: NVec -> IO ()
zeroCrossingHack v = do
  n <- nvLength v
  (flip mapM_) [0 .. (n - 1)] $ \i -> do
    d <- nvGet v i
    if abs d < 1.0E-12
       then nvSet v i 0
       else return ()