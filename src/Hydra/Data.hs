{-# LANGUAGE EmptyDataDecls, StandaloneDeriving, GADTs, FlexibleInstances, TemplateHaskell #-}

module Hydra.Data where

import Hydra.Utils.Impossible

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Maybe (isJust)

import Control.Monad

import Foreign
import Foreign.C.Types

data SR a where
  SR     :: (Signal a -> [Equation]) -> SR a
  Switch :: SR a -> SF a Bool -> (a -> SR a) -> SR a

data SF a b where
  SF     :: (Signal a -> Signal b) -> SF a b

data Equation where
  Local :: (Signal Double -> [Equation]) -> Equation
  Equal :: Signal Double -> Signal Double -> Equation
  Init  :: Signal Double -> Signal Double -> Equation
  App   :: SR a -> Signal a -> Equation

data Signal a where
  Unit    :: Signal ()
  Time    :: Signal Double
  Const   :: (Show a) => a -> Signal a
  Pair    :: (Show a, Show b) => Signal a -> Signal b -> Signal (a,b)
  PrimApp :: (Show a, Show b) => PrimSF a b -> Signal a -> Signal b
  Var     :: Int -> Signal Double

instance Eq (Signal a) where
  a == b = (show a == show b)

instance Show (Signal a) where
  show Unit           = "Unit"
  show Time           = "Time"
  show (Const a)      = "Const ("   ++ show a  ++ ")"
  show (Pair a b)     = "Pair ("    ++ show a  ++ ") (" ++ show b ++ ")"
  show (PrimApp sf s) = "PrimApp (" ++ show sf ++ ") (" ++ show s ++ ")"
  show (Var i)        = "Var ("     ++ show i  ++ ")"

data PrimSF a b where
  Der    :: PrimSF Double Double
  Not    :: PrimSF Bool Bool
  Or     :: PrimSF (Bool,Bool) Bool
  And    :: PrimSF (Bool,Bool) Bool
  Lt     :: PrimSF Double Bool
  Lte    :: PrimSF Double Bool
  Gt     :: PrimSF Double Bool
  Gte    :: PrimSF Double Bool
  Exp    :: PrimSF Double Double
  Sqrt   :: PrimSF Double Double
  Log    :: PrimSF Double Double
  Sin    :: PrimSF Double Double
  Tan    :: PrimSF Double Double
  Cos    :: PrimSF Double Double
  Asin   :: PrimSF Double Double
  Atan   :: PrimSF Double Double
  Acos   :: PrimSF Double Double
  Sinh   :: PrimSF Double Double
  Tanh   :: PrimSF Double Double
  Cosh   :: PrimSF Double Double
  Asinh  :: PrimSF Double Double
  Atanh  :: PrimSF Double Double
  Acosh  :: PrimSF Double Double
  Abs    :: PrimSF Double Double
  Sgn    :: PrimSF Double Double
  Neg    :: PrimSF Double Double
  Add    :: PrimSF (Double,Double) Double
  Sub    :: PrimSF (Double,Double) Double
  Mul    :: PrimSF (Double,Double) Double
  Div    :: PrimSF (Double,Double) Double
  Pow    :: PrimSF (Double,Double) Double

deriving instance Eq   (PrimSF a b)
deriving instance Show (PrimSF a b)

switch :: SR a -> SF a Bool -> (a -> SR a) -> SR a
switch = Switch

eval :: SymTab -> Signal a -> a
eval st e = case e of
  Unit                 -> ()
  Time                 -> timeCurrent st
  Const c              -> c
  Var   i1             -> Map.findWithDefault $impossible i1 (instants st)
  Pair a1 a2           -> (eval st a1,eval st a2)
  PrimApp Der (Var i1) -> Map.findWithDefault $impossible i1 (instantsDiff st)
  PrimApp Der _        -> error "This version of Hydra only supports first order derivatives."
  PrimApp sf1 e1       -> (evalPrimSF sf1) (eval st e1)

evalPrimSF :: PrimSF a b -> (a -> b)
evalPrimSF sf = case sf of
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
  Neg   -> negate
  Add   -> uncurry (+)
  Sub   -> uncurry (-)
  Mul   -> uncurry (*)
  Div   -> uncurry (/)
  Pow   -> uncurry (**)
  Lt    -> \d -> (d <  0)
  Lte   -> \d -> (d <= 0)
  Gt    -> \d -> (d >  0)
  Gte   -> \d -> (d >= 0)
  Or    -> uncurry (||)
  And   -> uncurry (&&)
  Not   -> not
  Der   -> $impossible

isVar :: Signal Double -> Bool
isVar (Var _) = True
isVar _       = False

isConst :: Signal Double -> Bool
isConst (Const _) = True
isConst _         = False

isConstZero :: Signal Double -> Bool
isConstZero (Const 0) = True
isConstZero _         = False

data SymTab = SymTab {
    model         :: [Equation]
  , variables     :: Map Int (Maybe Double)
  , events        :: [(Signal Bool,(Int,Bool))]
  , equations     :: [Signal Double]
  , timeCurrent   :: Double
  , instants      :: Map Int Double
  , instantsDiff  :: Map Int Double
  }

empty :: SymTab
empty = SymTab {
    model         = []
  , variables     = Map.empty
  , events        = []
  , equations     = []
  , timeCurrent   = 0
  , instants      = Map.empty
  , instantsDiff  = Map.empty
  }

variableNumber :: SymTab -> Int
variableNumber = Map.size . variables

variableNumberDiff :: SymTab -> Int
variableNumberDiff =
    length
  . filter isJust . Map.elems
  . variables

eventNumber :: SymTab -> Int
eventNumber = length . events

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
                                               -- Return value 0: soulution has been obtained succesfully
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