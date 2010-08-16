{-# LANGUAGE TemplateHaskell #-}

module Hydra.Stages.Flatten (flatten) where

import Hydra.Utils.Impossible (impossible)

import Hydra.Data

import qualified Data.Map as Map

flatten :: SymTab -> SymTab
flatten symtab = go (symtab{equations=[],variables = Map.empty}) (flattenEqs 0 (model symtab))
  where
  go :: SymTab -> [Equation] -> SymTab
  go acc []                         = acc
  go acc ((Equal e1 e2) : eqs)      = let e3 = simplify e1
                                          e4 = simplify e2
                                          acc1 = buildVars (buildVars acc e3) e4
                                      in  go (acc1 {equations = (evalCurs acc1 (e3 - e4)) : (equations acc1)}) eqs
  go acc ((Init  (Var i) e1) : eqs) = let e2 = simplify e1
                                          acc1 = buildVars acc e2
                                      in  go (acc1 {variables = Map.insert i (Just (eval acc1 e2)) (variables acc1)}) eqs
  go acc ((Init (App2 Mul (Const (-1)) e1) e2) : eqs) = go acc ((Init e1 (negate (simplify e2))) : eqs)
  go _   ((Init _ _) : _)           = error "This version of Hydra only supports directed (i.e., caulal) init and reinit equations."
  go acc ((Monitor (Var i)) : eqs) = go (acc {monitors = Map.insert i () (monitors acc)}) eqs
  go _   _                         = $impossible

flattenEqs :: Int -> [Equation] -> [Equation]
flattenEqs _ []         = []
flattenEqs i (eq : eqs) = case eq of
  App (SigRel f1) s1      -> flattenEqs i (f1 s1 ++ eqs)
  App (Switch sr1 _ _) s1 -> flattenEqs i ((App sr1 s1) : eqs)

  Local f1     -> flattenEqs (i + 1) (f1 (Var i) ++ eqs)
  Equal   _  _ -> eq : flattenEqs i eqs
  Init    _  _ -> eq : flattenEqs i eqs
  Reinit  _  _ ->      flattenEqs i eqs
  Monitor _    -> eq : flattenEqs i eqs

buildVars :: SymTab -> Signal Double -> SymTab
buildVars acc e = case e of
  Time          -> acc
  Const _       -> acc
  Var  i1       -> case Map.lookup i1 (variables acc) of
                     Nothing -> acc{variables = Map.insert i1 Nothing (variables acc)}
                     Just _  -> acc
  Der  (Var i1) -> case Map.lookup i1 (variables acc) of
                     Nothing -> acc{variables = Map.insert i1 (Just 0) (variables acc)}
                     Just _  -> acc
  Der  _        -> error "This version of Hydra only supports first order derivatives."
  Cur  e1       -> buildVars acc e1
  App1 _ e1     -> buildVars acc e1
  App2  _ e1 e2 -> buildVars (buildVars acc e1) e2

evalCurs :: SymTab -> Signal Double -> Signal Double
evalCurs st e = case e of
  Time -> e
  Const _ -> e
  Var   _ -> e
  Der   _ -> e
  Cur   e1 -> Const (eval st e1)
  App1  f1 e1 -> App1 f1 (evalCurs st e1)
  App2  f1 e1 e2 -> App2 f1 (evalCurs st e1) (evalCurs st e2)
 

simplify :: Signal Double -> Signal Double
simplify e = case e of
  Time    -> e
  Const _ -> e
  Var   _ -> e
  Der   (Var _) -> e
  Der (App2 Mul (Const (-1)) e1) -> negate (simplify (Der e1))
  Der _ -> error "This version of Hydra only supports first order derivatives."
  Cur   e1 -> Cur (simplify e1)
  App1  f1 e1 -> App1 f1 (simplify e1)
  App2  f1 e1 e2 -> App2 f1 (simplify e1) (simplify e2)

