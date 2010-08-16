module Hydra.Stages.Parse (parseSigRel,parseSigFun) where

import Hydra.BNFC.AbsHydra
import Hydra.BNFC.LexHydra (tokens)
import Hydra.BNFC.LayoutHydra (resolveLayout)
import Hydra.BNFC.ParHydra (pSigRel,pSigFun)
import Hydra.BNFC.ErrM (Err(Bad,Ok))

parseSigRel :: (Monad m) => (String,Int,Int) -> String -> m SigRel
parseSigRel (fn,l,c) s = case pSigRel (resolveLayout True (tokens s)) of
  Bad err -> fail (err ++ "; In file '" ++ fn ++ "' at " ++ show l ++ ":" ++ show c)
  Ok tree -> return tree

parseSigFun :: (Monad m) => (String,Int,Int) -> String -> m SigFun
parseSigFun (fn,l,c) s = case pSigFun (resolveLayout True (tokens s)) of
  Bad err -> fail (err ++ "; In file '" ++ fn ++ "' at " ++ show l ++ ":" ++ show c)
  Ok tree -> return tree