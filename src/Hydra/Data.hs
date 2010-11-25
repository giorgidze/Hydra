{-# LANGUAGE EmptyDataDecls, ExistentialQuantification, TypeFamilies, FlexibleInstances, TemplateHaskell #-}

module Hydra.Data where

import Hydra.Utils.Impossible

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Maybe (isJust)

import Control.Monad

import Foreign
import Foreign.C.Types

data SR a =
    SigRel !(Signal a -> [Equation])
  | Switch !(SR a) !(SF a Bool) !(a -> SR a)

data SF a b = SigFun !(Signal a -> Signal b)

data Equation =
    Local   !(Signal Double -> [Equation])
  | Equal   !(Signal Double) !(Signal Double)
  | Init    !(Signal Double) !(Signal Double)
  | Reinit  !(Signal Double) !(Signal Double)
  | forall a. (SignalType a) => App !(SR a) !(Signal a)
  | Monitor !(Signal Double)

switch :: SR a -> SF a Bool -> (a -> SR a) -> SR a
switch = Switch


class SignalType a where
  data Signal a
  eval :: SymTab -> Signal a -> a

instance SignalType () where
  data Signal () = Unit
  eval _ _ = ()

instance SignalType Double where
  data Signal Double = Time
                     | Const !Double
                     | Var   !Int
                     | Der   !(Signal Double)
                     | Cur   !(Signal Double)
                     | App1  !Func1 !(Signal Double)
                     | App2  !Func2 !(Signal Double) !(Signal Double)
                       deriving (Eq,Ord,Show)
  eval st e = evalSignalDouble st (evalNewToOldDouble st e)


der :: Signal Double -> Signal Double
der = Der

cur :: Signal Double -> Signal Double
cur = Cur


evalSignalDouble :: SymTab -> Signal Double -> Double
evalSignalDouble st e = case e of
  Time           -> timeCurrent st
  Const d1       -> d1
  Var   i1       -> Map.findWithDefault $impossible i1 (instants st)
  Der   (Var i1) -> Map.findWithDefault $impossible i1 (instantsDiff st)
  Der   _        -> error "This version of Hydra only supports first order derivatives."
  -- Der   e1       -> eval st (Der (Map.findWithDefault $impossible e1 (substs st)))
  Cur   e1       -> eval st e1
  App1  f1 e1    -> (evalFunc1 f1) (eval st e1)
  App2  f2 e1 e2 -> (evalFunc2 f2) (eval st e1) (eval st e2)

evalNewToOldDouble :: SymTab -> Signal Double -> Signal Double
evalNewToOldDouble st e = case e of
  Time           -> e
  Const _        -> e
  Var   i1       -> Var (Map.findWithDefault $impossible i1 (newToOld st))
  Der   e1       -> Der (evalNewToOldDouble st e1)
  Cur   e1       -> Cur (evalNewToOldDouble st e1)
  App1  f1 e1    -> App1 f1 (evalNewToOldDouble st e1)
  App2  f2 e1 e2 -> App2 f2 (evalNewToOldDouble st e1) (evalNewToOldDouble st e2)


ppSignalDouble :: Signal Double -> String
ppSignalDouble e = case e of
  Time           -> "time"
  Const d1       -> show d1
  Var   i1       -> 'v' : show i1
  Der   e1       -> "der" ++ " " ++ ppSignalDouble e1
  Cur   e1       -> "cur" ++ " " ++ ppSignalDouble e1
  App1  f1 e1    -> show f1 ++ " " ++ ppSignalDouble e1
  App2  f2 e1 e2 -> case f2 of
    Add -> "(" ++ ppSignalDouble e1 ++ " + " ++ ppSignalDouble e2 ++ ")"
    Mul -> "(" ++ ppSignalDouble e1 ++ " * " ++ ppSignalDouble e2 ++ ")"
    Div -> "(" ++ ppSignalDouble e1 ++ " / " ++ ppSignalDouble e2 ++ ")"
    Pow -> "(" ++ ppSignalDouble e1 ++ " ^ " ++ ppSignalDouble e2 ++ ")"


    

data Func1 =
    Exp
  | Sqrt
  | Log
  | Sin
  | Tan
  | Cos
  | Asin
  | Atan
  | Acos
  | Sinh
  | Tanh
  | Cosh
  | Asinh
  | Atanh
  | Acosh
  | Abs
  | Sgn
  deriving (Eq,Ord,Show)

data Func2 =
    Add
  | Mul
  | Div
  | Pow
  deriving (Eq,Ord,Show)


instance SignalType Bool where
  data Signal Bool = Or    !(Signal Bool) !(Signal Bool)
                   | And   !(Signal Bool) !(Signal Bool)
                   | Xor   !(Signal Bool) !(Signal Bool)
                   | Comp  !CompFun !(Signal Double)
                     deriving (Eq,Ord,Show)
  eval st e = evalSignalBool st (evalNewToOldBool st e)

evalSignalBool :: SymTab -> Signal Bool -> Bool
evalSignalBool st e = case e of
  Or  e1 e2   -> (evalSignalBool st e1) || (evalSignalBool st e2) 
  And e1 e2   -> (evalSignalBool st e1) && (evalSignalBool st e2) 
  Xor e1 e2   -> if (evalSignalBool st e1)
                    then  Prelude.not (evalSignalBool st e2)
                    else (evalSignalBool st e2)
  Comp  f1 e1 -> (evalCompFun f1) (evalSignalDouble st e1)

evalNewToOldBool :: SymTab -> Signal Bool -> Signal Bool
evalNewToOldBool st e = case e of
  Or  e1 e2   -> Or  (evalNewToOldBool st e1) (evalNewToOldBool st e2)
  And e1 e2   -> And (evalNewToOldBool st e1) (evalNewToOldBool st e2)
  Xor e1 e2   -> Xor (evalNewToOldBool st e1) (evalNewToOldBool st e2)
  Comp  f1 e1 -> Comp f1 (evalNewToOldDouble st e1)


data CompFun = Lt | Lte | Gt | Gte deriving (Eq,Ord,Show) 

instance (SignalType a1,SignalType a2) => SignalType (a1,a2) where
  data Signal (a1,a2) = Tuple2 !(Signal a1) !(Signal a2)
  eval st (Tuple2 a1 a2) = (eval st a1,eval st a2)

instance (SignalType a1,SignalType a2,SignalType a3) => SignalType (a1,a2,a3) where
  data Signal (a1,a2,a3) = Tuple3 !(Signal a1) !(Signal a2) !(Signal a3)
  eval st (Tuple3 a1 a2 a3) = (eval st a1,eval st a2,eval st a3)

instance (SignalType a1,SignalType a2,SignalType a3,SignalType a4) => SignalType (a1,a2,a3,a4) where
  data Signal (a1,a2,a3,a4) = Tuple4 !(Signal a1) !(Signal a2) !(Signal a3) !(Signal a4)
  eval st (Tuple4 a1 a2 a3 a4) = (eval st a1,eval st a2,eval st a3,eval st a4)


instance Num (Signal Double) where
  (+) e1 e2     = App2 Add e1 e2
  (*) e1 e2     = App2 Mul e1 e2
  (-) e1 e2     = App2 Add e1 ((Const (-1)) * e2)
  negate e1     = (Const (-1)) * e1
  abs e1        = App1 Abs e1
  signum e1     = App1 Sgn e1
  fromInteger i = Const (fromIntegral i)

instance Fractional (Signal Double) where
  (/) e1 e2 = App2 Div e1 e2
  recip e1 = 1 / e1
  fromRational r = Const (fromRational r)

instance Floating (Signal Double) where
  pi = Const pi
  exp   e1 = App1 Exp   e1
  log   e1 = App1 Log   e1
  sqrt  e1 = App1 Sqrt  e1
  sin   e1 = App1 Sin   e1
  cos   e1 = App1 Cos   e1
  tan   e1 = App1 Tan   e1
  asin  e1 = App1 Asin  e1
  acos  e1 = App1 Acos  e1
  atan  e1 = App1 Atan  e1
  sinh  e1 = App1 Sinh  e1
  cosh  e1 = App1 Cosh  e1
  tanh  e1 = App1 Tanh  e1
  asinh e1 = App1 Asinh e1
  acosh e1 = App1 Acosh e1
  atanh e1 = App1 Atanh e1
  (**) e1 e2 = App2 Pow e1 e2

isVar :: Signal Double -> Bool
isVar (Var _) = True
isVar _       = False

isConst :: Signal Double -> Bool
isConst (Const _) = True
isConst _          = False

isConstZero :: Signal Double -> Bool
isConstZero (Const 0) = True
isConstZero _            = False

evalFunc1 :: Func1 -> (Double -> Double)
evalFunc1 f = case f of
  Exp   -> exp
  Sqrt  -> sqrt
  Log   -> log
  Sin   -> sin
  Tan   -> tan
  Cos   -> cos
  Asin  -> asin
  Atan  -> atan
  Acos  -> acos
  Sinh  -> sinh
  Tanh  -> tanh
  Cosh  -> cosh
  Asinh -> asinh
  Atanh -> atanh
  Acosh -> acosh
  Abs   -> abs
  Sgn   -> signum

evalFunc2 :: Func2 -> (Double -> Double -> Double)
evalFunc2 f = case f of
  Add -> (+)
  Mul -> (*)
  Div -> (/)
  Pow -> (**)

not :: Signal Bool -> Signal Bool
not = Xor (Comp Gt (Const 1))

evalCompFun :: CompFun -> (Double -> Bool)
evalCompFun scf d = case scf of
  Lt  -> (d <  0)
  Lte -> (d <= 0)
  Gt  -> (d >  0) 
  Gte -> (d >= 0)

data SymTab = SymTab {
    model         :: [Equation]
  , variables     :: Map Int (Maybe Double)
  , events        :: Map (Signal Bool) (Int,Bool)
  , equations     :: [Signal Double]
  , newToOld      :: Map Int Int
  , substs        :: Map (Signal Double) (Signal Double)
  , timeCurrent   :: Double
  , instants      :: Map Int Double
  , instantsDiff  :: Map Int Double
  , monitors      :: Map Int ()
  }

empty :: SymTab
empty = SymTab {
    model         = []
  , variables     = Map.empty
  , events        = Map.empty
  , equations     = []
  , newToOld      = Map.empty
  , substs        = Map.empty
  , timeCurrent   = 0
  , instants      = Map.empty
  , instantsDiff  = Map.empty
  , monitors      = Map.empty
  }

variableNumber :: SymTab -> Int
variableNumber = Map.size . variables

variableNumberDiff :: SymTab -> Int
variableNumberDiff =
    length
  . filter isJust . Map.elems
  . variables

eventNumber :: SymTab -> Int
eventNumber = Map.size . events

equationNumber :: SymTab -> Int
equationNumber = length . equations

data Experiment = Experiment {
    timeStart      :: Double
  , timeStop       :: Double
  , timeStep       :: Double
  , jitCompile     :: Bool
  , visualise      :: CDouble -> CInt -> Ptr CDouble -> IO ()
  , solver         :: Solver
  }

data Solver = Solver {
    createSolver    :: CDouble      -- ^ Start time
                    -> CDouble      -- ^ Stop time
                    -> Ptr CDouble  -- ^ Current time
                    -> CInt         -- ^ Number of variables
                    -> Ptr CDouble  -- ^ Variables
                    -> Ptr CDouble  -- ^ Differentials
                    -> Ptr CInt     -- ^ Constrained differentials
                    -> CInt         -- ^ Number of events
                    -> Ptr CInt     -- ^ Events
                    -> Residual     -- ^ Initialisation equations
                    -> Residual     -- ^ Main equations
                    -> Residual     -- ^ Event Equations
                    -> IO SolverHandle
  , destroySolver   :: SolverHandle -> IO ()
  , solve           :: SolverHandle -> IO CInt -- ^
                                               -- Return value 0: soulution has been obtained usccesfully
                                               -- Return value 1: event has occurence
                                               -- Return value 2: stop time has been reached
  }

data Void
type SolverHandle = Ptr Void

type Residual = FunPtr (CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO Void)


experimentDefault :: Experiment
experimentDefault = Experiment {
    timeStart = 0
  , timeStop = 10
  , timeStep = 0.001
  , jitCompile = True
  , visualise = visualiseDump
  , solver = error "Solver has not been specified. You can use the solver defined in hydra-sundials package or provide your own."
  }
  
visualiseDump :: CDouble -> CInt -> Ptr CDouble -> IO ()
visualiseDump time len arr = do
  putStr (show time)
  putStr " "
  forM_ [0 .. (len - 1)] $ \i -> do
    d <- peekElemOff arr (fromIntegral i);
    putStr (show d)
    putStr " "
  putStrLn []