{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface #-}

module Hydra.Stages.Simulate (simulate) where

import Hydra.Data
import Hydra.Stages.BuildNewToOld
import Hydra.Stages.HandleEvents
import Hydra.Stages.BuildEvents
import Hydra.Stages.Flatten
import Hydra.Stages.Validate
import qualified Hydra.Stages.JIT as JIT (compile)

import qualified Data.Map as Map
import Data.Map (Map)

import Foreign
import Foreign.C.Types


simulate :: Experiment -> SR () -> IO ()
simulate exper sr = case sr of
  SR f1 -> simulateAux exper (empty{model = f1 Unit})
  sr1   -> simulate exper (SR (\_ -> [App sr1 Unit]))

simulateAux :: Experiment -> SymTab -> IO ()
simulateAux exper symbolTable = do
  let symtab = validate $ flatten
                        $ buildEvents
                        $ handleEvents
                        $ buildNewToOld
                        $ symbolTable

  let nVar     = variableNumber symtab
  let nVarEv   = eventNumber    symtab

  (llvmCleanup, residualInit, residualMain, residualEvent) <- JIT.compile exper symtab

  y   <- newArray (replicate nVar 0)
  yp  <- newArray (replicate nVar 0)
  idv <- newArray (replicate nVar 0)
  ev  <- newArray (replicate nVarEv 0)
  tp  <- new 0
  initialiseVectors symtab y idv

  handle <- (createSolver (solver exper)) (realToFrac (timeStart exper))
                                          (realToFrac (timeStop exper))
                                          tp
                                          (fromIntegral nVar) y yp idv
                                          (fromIntegral nVarEv) ev
                                          residualInit residualMain residualEvent


  let cleanup :: IO ()
      cleanup = do llvmCleanup; free y; free yp; free idv; free ev; free tp;
                   (destroySolver (solver exper)) handle;

  (visualise exper) (realToFrac (timeStart exper)) (fromIntegral nVar) y

  let loop :: IO ()
      loop = do
        ir <- (solve (solver exper)) handle
        case ir of
          0 -> do
            t <- peek tp
            (visualise exper) t (fromIntegral nVar) y
            poke tp (t + realToFrac (timeStep exper))
            loop
          1 -> do t <- peek tp
                  instants1     <- ptrToMap nVar y
                  instantsDiff1 <- ptrToMap nVar yp
                  ev1 <- peekArray nVarEv ev; print ev1;
                  (visualise exper) t (fromIntegral nVar) y

                  let f (sb,(i,_)) = if (ev1 !! i) /= 0 then (sb,(i,True)) else (sb,(i,False))

                  let symtabNew = symtab { instants = instants1
                                         , instantsDiff = instantsDiff1
                                         , events = fmap f (events symtab)
                                         }
                  cleanup
                  simulateAux (exper{timeStart = realToFrac t}) (symtabNew {timeCurrent = realToFrac t})
          _ -> do
            t <- peek tp
            (visualise exper) t (fromIntegral nVar) y


  poke tp (realToFrac (timeStart exper + timeStep exper))
  loop 

initialiseVectors :: SymTab -> Ptr CDouble -> Ptr CInt -> IO ()
initialiseVectors symtab y idv = mapM_ go $ Map.assocs $ fmap (fmap realToFrac) $ variables symtab
  where
  go :: (Int,Maybe CDouble) -> IO ()
  go (_,Nothing)  = return ()
  go (i,Just d)   = do pokeElemOff y   i d
                       pokeElemOff idv i 1
                       
ptrToMap :: (Storable a, Real a, Fractional b) => Int -> Ptr a -> IO (Map Int b)
ptrToMap i v = peekArray i v >>= return . Map.fromList . zip [0..] . map realToFrac
