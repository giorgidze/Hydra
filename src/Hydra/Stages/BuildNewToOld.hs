{-# LANGUAGE TemplateHaskell #-}

module Hydra.Stages.BuildNewToOld (buildNewToOld) where

import Hydra.Data

import qualified Data.Map as Map

buildNewToOld :: SymTab -> SymTab
buildNewToOld st = case ntoEqs True 0 True 0 st (model st) of
  (st1,_,_) -> st1

ntoEqs :: Bool -> Int -> Bool -> Int -> SymTab -> [Equation] -> (SymTab,Int,Int)
ntoEqs _      new _      old acc []         = (acc,new,old)
ntoEqs incNew new incOld old acc (eq : eqs) = case eq of
  Local f1 ->
    let acc1 = if (incNew && incOld)
                  then acc {newToOld = Map.insert new old (newToOld acc)}
                  else acc
        new1 = if incNew then new + 1 else new
        old1 = if incOld then old + 1 else old
    in  ntoEqs incNew new1 incOld old1 acc1 ((f1 (Var old)) ++ eqs)

  App (SigRel f1) s1 -> ntoEqs incNew new incOld old acc (f1 s1 ++ eqs)

  App (Switch sr1 (SigFun sf1) f2) s1 -> if incOld
    then case Map.lookup (sf1 s1) (events acc) of
           Nothing        -> ntoEqs incNew new incOld old acc ((App sr1 s1) : eqs)
           Just (_,False) -> ntoEqs incNew new incOld old acc ((App sr1 s1) : eqs)
           Just (_,True)  -> let (acc1,new1,old1) = ntoEqs False new  True  old  acc  [App sr1 s1]
                                 (acc2,new2,old2) = ntoEqs True  new1 False old1 acc1 [App (f2 (eval acc1 s1)) s1]
                             in  ntoEqs True new2 True old2 acc2 eqs
    else ntoEqs incNew new incOld old acc ((App sr1 s1) : eqs)

  Equal  _ _ -> ntoEqs incNew new incOld old acc eqs
  Init   _ _ -> ntoEqs incNew new incOld old acc eqs
  Reinit _ _ -> ntoEqs incNew new incOld old acc eqs
  Monitor _  -> ntoEqs incNew new incOld old acc eqs