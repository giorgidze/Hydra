module Hydra.Stages.HandleEvents (handleEvents) where

import Hydra.Data

handleEvents :: SymTab -> SymTab
handleEvents st =
  let evs = map fst $ filter (snd . snd) $ events st
  in  if null evs
         then st
         else st {model = handleEvs st evs (model st)}

handleEvs :: SymTab -> [Signal Bool] -> [Equation] -> [Equation]
handleEvs _ _ [] = []
handleEvs st evs (eq : eqs) = case eq of
  Local f1 -> Local (\s -> handleEvs st evs (f1 s)) : handleEvs st evs eqs

  App (SigRel f1) s1 -> App (SigRel (\s -> handleEvs st evs (f1 s))) s1 : handleEvs st evs eqs

  App (Switch sr1 (SigFun sf1) f2) s1 ->
     if elem (sf1 s1) evs
        then App (f2 (eval st s1)) s1 : handleEvs st evs eqs
        else App (Switch (SigRel (\_ -> handleEvs st evs [App sr1 s1])) (SigFun sf1) f2) s1 : handleEvs st evs eqs

  Equal  _ _  ->            eq : handleEvs st evs eqs
  Init   _ _  ->                 handleEvs st evs eqs
