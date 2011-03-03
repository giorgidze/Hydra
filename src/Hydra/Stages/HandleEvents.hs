module Hydra.Stages.HandleEvents (handleEvents) where

import Hydra.Data

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

handleEvents :: SymTab -> SymTab
handleEvents st =
  let evs = Map.keysSet $ Map.filter snd $ events st
  in  if Set.null evs
         then st
         else st {model = handleEvs st evs (model st)}

handleEvs :: SymTab -> Set (Signal Bool) -> [Equation] -> [Equation]
handleEvs _ _ [] = []
handleEvs st evs (eq : eqs) = case eq of
  Local f1 -> Local (\s -> handleEvs st evs (f1 s)) : handleEvs st evs eqs

  App (SigRel f1) s1 -> App (SigRel (\s -> handleEvs st evs (f1 s))) s1 : handleEvs st evs eqs

  App (Switch sr1 (SigFun sf1) f2) s1 ->
     if Set.member (sf1 s1) evs
        then App (f2 (eval st s1)) s1 : handleEvs st evs eqs
        else App (Switch (SigRel (\_ -> handleEvs st evs [App sr1 s1])) (SigFun sf1) f2) s1 : handleEvs st evs eqs

  Equal  _ _  ->            eq : handleEvs st evs eqs
  Init   _ _  ->                 handleEvs st evs eqs
  Reinit v e  -> Init v e : eq : handleEvs st evs eqs
