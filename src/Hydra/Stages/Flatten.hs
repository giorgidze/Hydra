{-# LANGUAGE TemplateHaskell, GADTs #-}

module Hydra.Stages.Flatten (flatten) where

import Hydra.Utils.Impossible (impossible)

import Hydra.Data

import qualified Data.Map as Map

flatten :: SymTab -> SymTab
flatten symtab = go (symtab{equations=[],variables = Map.empty}) (flattenEqs 0 (model symtab))
  where
  go :: SymTab -> [Equation] -> SymTab
  go acc []                         = acc
  go acc ((Equal e1 e2) : eqs)      = let acc1 = buildVars (buildVars acc e1) e2
                                      in  go (acc1 {equations = (PrimApp Sub (Pair e1 e2)) : (equations acc1)}) eqs
  go acc ((Init  (Var i) e1) : eqs) = let acc1 = buildVars acc e1
                                      in  go (acc1 {variables = Map.insert i (Just (eval acc1 e1)) (variables acc1)}) eqs
  go _   ((Init _ _) : _)           = error "This version of Hydra only supports directed (i.e., caulal) init and reinit equations."
  go _   _                          = $impossible

flattenEqs :: Int -> [Equation] -> [Equation]
flattenEqs _ []         = []
flattenEqs i (eq : eqs) = case eq of
  App (SR f1) s1          -> flattenEqs i (f1 s1 ++ eqs)
  App (Switch sr1 _ _) s1 -> flattenEqs i ((App sr1 s1) : eqs)

  Local f1     -> flattenEqs (i + 1) (f1 (Var i) ++ eqs)
  Equal   _  _ -> eq : flattenEqs i eqs
  Init    _  _ -> eq : flattenEqs i eqs

buildVars :: SymTab -> Signal a -> SymTab
buildVars acc e = case e of
  Unit          -> acc
  Time          -> acc
  Const _       -> acc
  Var  i1       -> case Map.lookup i1 (variables acc) of
                     Nothing -> acc{variables = Map.insert i1 Nothing (variables acc)}
                     Just _  -> acc

  PrimApp Der  (Var i1) -> case Map.lookup i1 (variables acc) of
                             Nothing -> acc{variables = Map.insert i1 (Just 0) (variables acc)}
                             Just _  -> acc
  PrimApp Der  _        -> error "This version of Hydra only supports first order derivatives."

  PrimApp _ e1 -> buildVars acc e1
  Pair e1 e2 -> buildVars (buildVars acc e1) e2
