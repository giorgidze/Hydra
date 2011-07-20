{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Hydra.Stages.Quote (rel,fun) where

import qualified Hydra.BNFC.AbsHydra as BNFC
import Hydra.Data
import Hydra.Stages.Parse (parseSigRel,parseSigFun)
import Hydra.Stages.Desugar (desugar)

import Language.Haskell.Meta (parseExp)

import qualified Language.Haskell.TH.Quote as QQ (QuasiQuoter(..))
import qualified Language.Haskell.TH as TH

rel  :: QQ.QuasiQuoter
rel  =  QQ.QuasiQuoter { QQ.quoteExp = quoteSigRel}

fun  :: QQ.QuasiQuoter
fun  =  QQ.QuasiQuoter {QQ.quoteExp = quoteSigFun}

quoteSigRel :: String -> TH.ExpQ
quoteSigRel s = do
  loc <- TH.location
  let pos =  (TH.loc_filename loc, fst (TH.loc_start loc), snd (TH.loc_start loc))
  sr <- parseSigRel pos s
  case desugar sr of
    BNFC.SigRel pat1 eqs1 -> [| SR $(TH.lamE [quotePattern pat1] (quoteEquations eqs1)) |]

quoteSigFun :: String -> TH.ExpQ
quoteSigFun s = do
  loc <- TH.location
  let pos =  (TH.loc_filename loc, fst (TH.loc_start loc), snd (TH.loc_start loc))
  sf <- parseSigFun pos s
  case sf of
    BNFC.SigFun pat1 be1 -> [| SF $(TH.lamE [quotePattern pat1] (quoteExpr be1)) |]

quotePattern :: BNFC.Pattern -> TH.PatQ
quotePattern pat = case pat of
  BNFC.PatWild -> TH.wildP
  BNFC.PatUnit -> TH.conP (TH.mkName "Unit") []
  BNFC.PatName (BNFC.Ident s1) -> TH.varP (TH.mkName s1)
  BNFC.PatPair pat1 pat2 -> TH.conP (TH.mkName "Pair") [quotePattern pat1, quotePattern pat2]

quoteEquations :: [BNFC.Equation] -> TH.ExpQ
quoteEquations [] = [| [] |]
quoteEquations (eq : eqs) = case eq of
  BNFC.EquSigRelApp (BNFC.HsExpr s1) e1 -> case parseExp (init (tail s1)) of
    Left s2   -> fail s2
    Right sr1 -> [| (App  $(return sr1) $(quoteExpr e1)) : $(quoteEquations eqs) |]

  BNFC.EquEqual  e1 e2 -> [| (Equal  $(quoteExpr e1) $(quoteExpr e2)) : $(quoteEquations eqs) |]
  BNFC.EquInit   e1 e2 -> [| (Init   $(quoteExpr e1) $(quoteExpr e2)) : $(quoteEquations eqs) |]

  BNFC.EquLocal (BNFC.Ident s1) -> [| [Local ( $(TH.lamE [TH.varP (TH.mkName s1)] (quoteEquations eqs)) )] |]

quoteExpr :: BNFC.Expr -> TH.ExpQ
quoteExpr e = case e of
  BNFC.ExprAnti (BNFC.HsExpr s1) -> case parseExp (init (tail s1)) of
    Left s2  -> fail s2
    Right e1 -> [| Const $(return e1) |]

  BNFC.ExprVar (BNFC.Ident "time")   -> [| Time           |]
  BNFC.ExprVar (BNFC.Ident "der")    -> [| PrimApp Der    |]
  BNFC.ExprVar (BNFC.Ident "exp")    -> [| PrimApp Exp    |]
  BNFC.ExprVar (BNFC.Ident "sqrt")   -> [| PrimApp Sqrt   |]
  BNFC.ExprVar (BNFC.Ident "log")    -> [| PrimApp Log    |]
  BNFC.ExprVar (BNFC.Ident "sin")    -> [| PrimApp Sin    |]
  BNFC.ExprVar (BNFC.Ident "tan")    -> [| PrimApp Tan    |]
  BNFC.ExprVar (BNFC.Ident "cos")    -> [| PrimApp Cos    |]
  BNFC.ExprVar (BNFC.Ident "asin")   -> [| PrimApp Asin   |]
  BNFC.ExprVar (BNFC.Ident "atan")   -> [| PrimApp Atan   |]
  BNFC.ExprVar (BNFC.Ident "acos")   -> [| PrimApp Acos   |]
  BNFC.ExprVar (BNFC.Ident "sinh")   -> [| PrimApp Sinh   |]
  BNFC.ExprVar (BNFC.Ident "tanh")   -> [| PrimApp Tanh   |]
  BNFC.ExprVar (BNFC.Ident "cosh")   -> [| PrimApp Cosh   |]
  BNFC.ExprVar (BNFC.Ident "asinh")  -> [| PrimApp Asinh  |]
  BNFC.ExprVar (BNFC.Ident "atanh")  -> [| PrimApp Atanh  |]
  BNFC.ExprVar (BNFC.Ident "acosh")  -> [| PrimApp Acosh  |]
  BNFC.ExprVar (BNFC.Ident "abs")    -> [| PrimApp Abs    |]
  BNFC.ExprVar (BNFC.Ident "signum") -> [| PrimApp Sgn    |]

  BNFC.ExprVar (BNFC.Ident s1)       -> TH.varE (TH.mkName s1)

  BNFC.ExprAdd e1 e2 -> [| PrimApp Add (Pair $(quoteExpr e1)  $(quoteExpr e2)) |]
  BNFC.ExprSub e1 e2 -> [| PrimApp Sub (Pair $(quoteExpr e1)  $(quoteExpr e2)) |]
  BNFC.ExprDiv e1 e2 -> [| PrimApp Div (Pair $(quoteExpr e1)  $(quoteExpr e2)) |]
  BNFC.ExprMul e1 e2 -> [| PrimApp Mul (Pair $(quoteExpr e1)  $(quoteExpr e2)) |]
  BNFC.ExprPow e1 e2 -> [| PrimApp Pow (Pair $(quoteExpr e1)  $(quoteExpr e2)) |]
  BNFC.ExprNeg e1    -> [| PrimApp Neg $(quoteExpr e1) |]

  BNFC.ExprApp (BNFC.ExprAnti (BNFC.HsExpr s1)) e2 ->case parseExp (init (tail s1)) of
    Left s2  -> fail s2
    Right e1 -> TH.appE [| case $(return e1) of SF f -> f |] (quoteExpr e2)
  BNFC.ExprApp e1 e2 -> TH.appE (quoteExpr e1) (quoteExpr e2)

  BNFC.ExprInteger i1  -> [| Const ((fromIntegral i1) :: Double) |]
  BNFC.ExprDouble d1   -> [| Const $(TH.litE (TH.rationalL (toRational d1))) |]
  BNFC.ExprUnit        -> [| Unit |]
  BNFC.ExprPair e1 e2  -> TH.appE (TH.appE (TH.conE (TH.mkName "Pair")) (quoteExpr e1)) (quoteExpr e2)