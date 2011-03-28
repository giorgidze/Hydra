{-# LANGUAGE TemplateHaskell #-}

module Hydra.Stages.Desugar (desugar) where

import Hydra.BNFC.AbsHydra
import Hydra.Utils.Impossible (impossible)

desugar :: SigRel -> SigRel
desugar = desugarFlowSigRel . desugarConnectSigRel . desugarTupleSigRel . desugarLocalSigRel


desugarLocalSigRel :: SigRel -> SigRel
desugarLocalSigRel sr = case sr of
  SigRel pat1 eqs1 -> SigRel pat1 (concatMap desugarLocalEquation eqs1)

desugarLocalEquation :: Equation -> [Equation]
desugarLocalEquation eq = case eq of
  EquSigRelApp _ _ -> [eq]
  EquEqual _ _ -> [eq]
  EquInit _ _ -> [eq]
  EquConnect _ _ _ -> [eq]
  EquConnectFlow _ _ _ -> [eq]
  EquLocal _ [] -> [eq]
  EquLocal li1 (li2 : lis) ->
    (EquLocal li1 []) : desugarLocalEquation (EquLocal li2 lis)


desugarTupleSigRel :: SigRel -> SigRel
desugarTupleSigRel sr = case sr of
  SigRel pat1 eqs1 -> SigRel pat1 (concatMap desugarTupleEquation eqs1)

desugarTupleEquation :: Equation -> [Equation]
desugarTupleEquation eq = case eq of
  EquSigRelApp _ _ -> [eq]
  EquConnect _ _ _ -> [eq]
  EquConnectFlow _ _ _ -> [eq]
  EquLocal _ _ -> [eq]

  EquEqual (ExprPair e1 e2) (ExprPair e3 e4) ->
    concatMap desugarTupleEquation [EquEqual e1 e3,EquEqual e2 e4]
  EquEqual _ _ -> [eq]

  EquInit (ExprPair e1 e2) (ExprPair e3 e4) ->
    concatMap desugarTupleEquation [EquInit e1 e3,EquInit e2 e4]
  EquInit _ _ -> [eq]

desugarConnectSigRel :: SigRel -> SigRel
desugarConnectSigRel sr = case sr of
  SigRel pat1 eqs1 ->  SigRel pat1 (concatMap desugarConnectEquation eqs1)

desugarConnectEquation :: Equation -> [Equation]
desugarConnectEquation eq = case eq of
  EquSigRelApp _ _ -> [eq]
  EquEqual _ _ -> [eq]
  EquInit _ _ -> [eq]
  EquLocal _ _ -> [eq]

  EquConnect li1 li2 lis ->
    let vs = map ExprVar (li1 : li2 : lis)
    in  zipWith EquEqual vs (tail vs)
  EquConnectFlow li1 li2 lis ->
    let vs = map ExprVar (li1 : li2 : lis)
    in  [EquEqual  (foldr1 (\e1 e2 -> ExprAdd e1 e2) vs) (ExprReal 0.0)]


desugarFlowSigRel :: SigRel -> SigRel
desugarFlowSigRel (SigRel pat1 eqs1) =
  let flowVars = desugarFlowFindPattern pat1
      pat2 = desugarFlowForgetPattern pat1
      eqs2 = foldr (\s eqs -> desugarFlowEquations s eqs) eqs1 flowVars
  in  SigRel pat2 eqs2

desugarFlowFindPattern :: Pattern -> [String]
desugarFlowFindPattern pat = case pat of
  PatWild -> []
  PatUnit -> []
  PatName QualEmpty _ -> []
  PatName QualFlow  (Ident s1) -> [s1]
  PatPair pat1 pat2 -> desugarFlowFindPattern pat1 ++ desugarFlowFindPattern pat2

desugarFlowForgetPattern :: Pattern -> Pattern
desugarFlowForgetPattern pat = case pat of
  PatWild -> pat
  PatUnit -> pat
  PatName QualEmpty _ -> pat
  PatName QualFlow  li1 -> PatName QualEmpty li1
  PatPair pat1 pat2 -> PatPair (desugarFlowForgetPattern pat1) (desugarFlowForgetPattern pat2)

desugarFlowEquations :: String -> [Equation] -> [Equation]
desugarFlowEquations _ [] = []
desugarFlowEquations s (eq : eqs) =
  let go :: Expr -> Expr
      go = desugarFlowExpr s
  in  case eq of
        EquSigRelApp hsExpr1 e1 ->
          (EquSigRelApp hsExpr1 (go e1)) : desugarFlowEquations s eqs
        EquEqual e1 e2          ->
          (EquEqual (go e1) (go e2))     : desugarFlowEquations s eqs
        EquInit e1 e2           ->
          (EquInit (go e1) (go e2))      : desugarFlowEquations s eqs
        EquLocal (Ident s1) [] ->
          if s1 == s
             then (eq : eqs)
             else  eq : desugarFlowEquations s eqs
        EquLocal _ _ -> $impossible
        EquConnect _ _ _ -> $impossible
        EquConnectFlow _ _ _ -> $impossible

desugarFlowExpr :: String -> Expr -> Expr
desugarFlowExpr s expr = go expr
  where
  go :: Expr -> Expr
  go e = case e of
    ExprVar (Ident s1) -> if s1 == s then ExprNeg e else e

    ExprAdd e1 e2 -> ExprAdd (go e1) (go e2)
    ExprSub e1 e2 -> ExprSub (go e1) (go e2)
    ExprDiv e1 e2 -> ExprDiv (go e1) (go e2)
    ExprMul e1 e2 -> ExprMul (go e1) (go e2)
    ExprPow e1 e2 -> ExprPow (go e1) (go e2)
    ExprOr  e1 e2 -> ExprOr  (go e1) (go e2)
    ExprAnd e1 e2 -> ExprAnd (go e1) (go e2)
    ExprLt  e1 e2 -> ExprLt  (go e1) (go e2)
    ExprLte e1 e2 -> ExprLte (go e1) (go e2)
    ExprGt  e1 e2 -> ExprGt  (go e1) (go e2)
    ExprGte e1 e2 -> ExprGte (go e1) (go e2)
    ExprApp v1 e1 -> ExprApp v1 (go e1)
    ExprNeg e1    -> ExprNeg (go e1)

    ExprPair e1 e2 -> ExprPair (go e1) (go e2)

    ExprAnti _ -> e
    ExprInt  _ -> e
    ExprReal _ -> e
    ExprUnit   -> e