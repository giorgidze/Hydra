{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Hydra.Stages.Quote (rel,fun) where

import Hydra.Utils.Impossible (impossible)
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
    BNFC.SigRel pat1 eqs1 -> [| SigRel $(TH.lamE [quotePattern pat1] (quoteEquations eqs1)) |]

quoteSigFun :: String -> TH.ExpQ
quoteSigFun s = do
  loc <- TH.location
  let pos =  (TH.loc_filename loc, fst (TH.loc_start loc), snd (TH.loc_start loc))
  sf <- parseSigFun pos s
  case sf of
    BNFC.SigFun pat1 be1 -> [| SigFun $(TH.lamE [quotePattern pat1] (quoteExpr be1)) |]

quotePattern :: BNFC.Pattern -> TH.PatQ
quotePattern pat = case pat of
  BNFC.PatternWild -> TH.wildP
  BNFC.PatternName _ (BNFC.LIdent s1) -> TH.varP (TH.mkName s1)
  BNFC.PatternTuple [] -> TH.conP (TH.mkName "Unit") []
  BNFC.PatternTuple [pat1] -> quotePattern pat1
  BNFC.PatternTuple pats -> TH.conP (TH.mkName ("Tuple" ++ show (length pats))) (map quotePattern pats)

quoteEquations :: [BNFC.Equation] -> TH.ExpQ
quoteEquations [] = [| [] |]
quoteEquations (eq : eqs) = case eq of
  BNFC.EquationSigRelApp (BNFC.HsExpr s1) e1 -> case parseExp (init (tail s1)) of
    Left s2   -> fail s2
    Right sr1 -> [| (App  $(return sr1) $(quoteExpr e1)) : $(quoteEquations eqs) |]

  BNFC.EquationEqual  e1 e2 -> [| (Equal  $(quoteExpr e1) $(quoteExpr e2)) : $(quoteEquations eqs) |]
  BNFC.EquationInit   e1 e2 -> [| (Init   $(quoteExpr e1) $(quoteExpr e2)) : $(quoteEquations eqs) |]

  BNFC.EquationLocal (BNFC.LIdent s1) [] -> [| [Local ( $(TH.lamE [TH.varP (TH.mkName s1)] (quoteEquations eqs)) )] |]

  BNFC.EquationLocal       _ _   -> $impossible
  BNFC.EquationConnect     _ _ _ -> $impossible
  BNFC.EquationConnectFlow _ _ _ -> $impossible

quoteExpr :: BNFC.Expr -> TH.ExpQ
quoteExpr e = case e of
  BNFC.ExprAnti (BNFC.HsExpr s1) -> case parseExp (init (tail s1)) of
    Left s2  -> fail s2
    Right e1 -> [| Const $(return e1) |]

  BNFC.ExprVar (BNFC.LIdent "time")  -> [| Time |]
  BNFC.ExprVar (BNFC.LIdent "not")   -> [| Hydra.Data.not    |]
  BNFC.ExprVar (BNFC.LIdent "true")  -> [| Comp Gt (Const 1) |]
  BNFC.ExprVar (BNFC.LIdent "false") -> [| Comp Lt (Const 1) |]  
  BNFC.ExprVar (BNFC.LIdent s1)      -> TH.varE (TH.mkName s1)

  BNFC.ExprAdd e1 e2 -> [| $(quoteExpr e1) +  $(quoteExpr e2) :: Signal Double |]
  BNFC.ExprSub e1 e2 -> [| $(quoteExpr e1) -  $(quoteExpr e2) :: Signal Double |]
  BNFC.ExprDiv e1 e2 -> [| $(quoteExpr e1) /  $(quoteExpr e2) :: Signal Double |]
  BNFC.ExprMul e1 e2 -> [| $(quoteExpr e1) *  $(quoteExpr e2) :: Signal Double |]
  BNFC.ExprPow e1 e2 -> [| $(quoteExpr e1) ** $(quoteExpr e2) :: Signal Double |]
  BNFC.ExprNeg e1 -> [| negate $(quoteExpr e1) :: Signal Double |]
  BNFC.ExprApp e1 e2 -> TH.appE (quoteExpr e1) (quoteExpr e2)

  BNFC.ExprInt i1  -> [| Const (fromIntegral (i1 :: Integer)) |]
  BNFC.ExprReal d1 -> [| Const $(TH.litE (TH.rationalL (toRational d1))) |]
  BNFC.ExprTuple [] -> [| Unit |]
  BNFC.ExprTuple [e1] -> quoteExpr e1
  BNFC.ExprTuple (e1 : e2 : es) -> foldl TH.appE (TH.conE (TH.mkName ("Tuple" ++ show (length es + 2)))) ((quoteExpr e1) : (quoteExpr e2) : (map quoteExpr es))

  BNFC.ExprOr  be1 be2 -> [| Or   $(quoteExpr be1) $(quoteExpr be2) |]
  BNFC.ExprAnd be1 be2 -> [| And  $(quoteExpr be1) $(quoteExpr be2) |]
  BNFC.ExprLt  e1  e2  -> [| Comp Lt  ($(quoteExpr  e1) - $(quoteExpr e2)) |]
  BNFC.ExprLte e1  e2  -> [| Comp Lte ($(quoteExpr  e1) - $(quoteExpr e2)) |]
  BNFC.ExprGt  e1  e2  -> [| Comp Gt  ($(quoteExpr  e1) - $(quoteExpr e2)) |]
  BNFC.ExprGte e1  e2  -> [| Comp Gte ($(quoteExpr  e1) - $(quoteExpr e2)) |]
