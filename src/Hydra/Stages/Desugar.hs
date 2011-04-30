module Hydra.Stages.Desugar (desugar) where

import Hydra.BNFC.AbsHydra

desugar :: SigRel -> SigRel
desugar = desugarTupleSigRel

desugarTupleSigRel :: SigRel -> SigRel
desugarTupleSigRel sr = case sr of
  SigRel pat1 eqs1 -> SigRel pat1 (concatMap desugarTupleEquation eqs1)

desugarTupleEquation :: Equation -> [Equation]
desugarTupleEquation eq = case eq of
  EquSigRelApp _ _ -> [eq]
  EquLocal _ -> [eq]

  EquEqual (ExprPair e1 e2) (ExprPair e3 e4) ->
    concatMap desugarTupleEquation [EquEqual e1 e3,EquEqual e2 e4]
  EquEqual _ _ -> [eq]

  EquInit (ExprPair e1 e2) (ExprPair e3 e4) ->
    concatMap desugarTupleEquation [EquInit e1 e3,EquInit e2 e4]
  EquInit _ _ -> [eq]