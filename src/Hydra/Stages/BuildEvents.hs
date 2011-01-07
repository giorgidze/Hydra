module Hydra.Stages.BuildEvents (buildEvents) where

import Hydra.Data

import qualified Data.Map as Map

buildEvents :: SymTab -> SymTab
buildEvents st = buildEvs 0 (st {events = Map.empty}) (model st)

buildEvs :: Int -> SymTab -> [Equation] -> SymTab
buildEvs _ acc []         = acc
buildEvs i acc (eq : eqs) = case eq of
  App (SigRel f1) s1 -> buildEvs i acc (f1 s1 ++ eqs)

  App (Switch sr1 (SigFun sf1) _) s1 ->
      let acc1 = acc {events = Map.insert (sf1 s1) (eventNumber acc,False) (events acc)}
      in  buildEvs i acc1 ((App sr1 s1) : eqs)

  Local f1   -> buildEvs (i + 1) acc (f1 (Var i) ++ eqs)
  Equal  _ _ -> buildEvs i acc eqs
  Init   _ _ -> buildEvs i acc eqs
  Reinit _ _ -> buildEvs i acc eqs
  Monitor _  -> buildEvs i acc eqs