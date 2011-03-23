{-# LANGUAGE TemplateHaskell, GADTs #-}

module Hydra.Stages.ToLLVM (compile) where

import qualified Paths_Hydra as Cabal

import Hydra.Utils.Impossible (impossible)
import qualified Hydra.Utils.LLVM as LLVM (readBitcode)

import Hydra.Data

import qualified LLVM.FFI.Core as LLVM

import qualified Foreign as FFI
import qualified Foreign.C.String as FFI

import Control.Monad.State.Strict
import qualified Data.List as List

type C a = StateT Context IO a

data Context = Context {
    symtab        :: !SymTab
  , params        :: !Params
  , modRef     :: !LLVM.ModuleRef
  , functionRef   :: !LLVM.ValueRef
  , basicBlockRef :: !LLVM.BasicBlockRef
  , builderRef    :: !LLVM.BuilderRef
  }

data Params = Params { tRef     :: !LLVM.ValueRef
                     , yVecRef  :: !LLVM.ValueRef
                     , ypVecRef :: !LLVM.ValueRef
                     , rVecRef  :: !LLVM.ValueRef
                     } deriving (Eq,Show)

compile :: SymTab -> IO LLVM.ModuleRef
compile st = do
  let initialContext = Context { symtab = st
                               , params = Params FFI.nullPtr FFI.nullPtr FFI.nullPtr FFI.nullPtr
                               , modRef = FFI.nullPtr
                               , functionRef = FFI.nullPtr
                               , basicBlockRef = FFI.nullPtr
                               , builderRef = FFI.nullPtr
                               }
  c <- execStateT compileModuleRef initialContext
  return (modRef c)

compileModuleRef :: C ()
compileModuleRef = do

  bitcodeFile <- lift (Cabal.getDataFileName "cbits/hydra_llvm.bc")
  modRef1 <- lift (LLVM.readBitcode bitcodeFile)

  modify (\c -> c {modRef = modRef1})

  builderRef1 <- lift $ LLVM.createBuilder
  modify (\c -> c {builderRef = builderRef1})

  compileFunctionDeclarations
  compileEquation
  compileEventEquation
  cleanupBuilder


compileFunctionDeclarations :: C ()
compileFunctionDeclarations = mapM_ (\(r,n,a) -> compileFunctionDeclaration r n a) functionDeclarations

voidPointerType :: LLVM.TypeRef
voidPointerType = LLVM.pointerType LLVM.int8Type 0

residualArgType :: [LLVM.TypeRef]
residualArgType = [ LLVM.doubleType
                  , LLVM.pointerType LLVM.doubleType 0
                  , LLVM.pointerType LLVM.doubleType 0
                  , LLVM.pointerType LLVM.doubleType 0
                  ]

functionDeclarations :: [(LLVM.TypeRef,String,[LLVM.TypeRef])]
functionDeclarations = [
    (LLVM.doubleType, "exp",   [LLVM.doubleType])
  , (LLVM.doubleType, "log",   [LLVM.doubleType])
  , (LLVM.doubleType, "sqrt",  [LLVM.doubleType])
  , (LLVM.doubleType, "sin",   [LLVM.doubleType])
  , (LLVM.doubleType, "cos",   [LLVM.doubleType])
  , (LLVM.doubleType, "tan",   [LLVM.doubleType])
  , (LLVM.doubleType, "asin",  [LLVM.doubleType])
  , (LLVM.doubleType, "acos",  [LLVM.doubleType])
  , (LLVM.doubleType, "atan",  [LLVM.doubleType])
  , (LLVM.doubleType, "sinh",  [LLVM.doubleType])
  , (LLVM.doubleType, "cosh",  [LLVM.doubleType])
  , (LLVM.doubleType, "tanh",  [LLVM.doubleType])
  , (LLVM.doubleType, "asinh", [LLVM.doubleType])
  , (LLVM.doubleType, "acosh", [LLVM.doubleType])
  , (LLVM.doubleType, "atanh", [LLVM.doubleType])
  , (LLVM.doubleType, "fabs",  [LLVM.doubleType])
  , (LLVM.doubleType, "pow",   [LLVM.doubleType, LLVM.doubleType])

  , (LLVM.voidType, "hydra_residual_main",  residualArgType)
  , (LLVM.voidType, "hydra_residual_event", residualArgType)
  ]

compileFunctionDeclaration :: LLVM.TypeRef -> String -> [LLVM.TypeRef] -> C ()
compileFunctionDeclaration retType name argTypes = do
  modRef1 <- return . modRef  =<< get
  funcType <- compileFunctionType retType argTypes
  _ <- lift $ FFI.withCString name $ \s -> LLVM.addFunction modRef1 s funcType
  return ()

compileFunctionType :: LLVM.TypeRef -> [LLVM.TypeRef] -> C LLVM.TypeRef
compileFunctionType retType argTypes = do
  lift $ FFI.withArray argTypes
       $ \a -> return (LLVM.functionType retType a (fromIntegral $ length argTypes) 0)

compileEquation :: C ()
compileEquation = do
  functionRef1 <- getFunctionRef "hydra_residual_main"
  modify $ \c -> c {functionRef = functionRef1}

  compileBasicBlockEntry
  
  let params1 = Params { tRef     = LLVM.getParam functionRef1 0
                       , yVecRef  = LLVM.getParam functionRef1 1
                       , ypVecRef = LLVM.getParam functionRef1 2
                       , rVecRef  = LLVM.getParam functionRef1 3
                       }
  modify $ \c -> c {params = params1}

  symtab1 <- return . symtab  =<< get

  let exprs = equations symtab1
  -- compileSigs exprs
  compileSigsAux $ zip [0..] exprs

  builderRef1 <- return . builderRef =<< get
  _ <- lift $ LLVM.buildRetVoid builderRef1

  return ()

compileEventEquation :: C ()
compileEventEquation = do
  functionRef1 <- getFunctionRef "hydra_residual_event"
  modify $ \c -> c {functionRef = functionRef1}

  compileBasicBlockEntry

  let params1 = Params { tRef     = LLVM.getParam functionRef1 0
                       , yVecRef  = LLVM.getParam functionRef1 1
                       , ypVecRef = LLVM.getParam functionRef1 2
                       , rVecRef  = LLVM.getParam functionRef1 3
                       }
  modify $ \c -> c {params = params1}

  evs1 <- return . events . symtab =<< get

  compileEvents $ map snd
                $ List.sortBy (\a b -> compare (fst a) (fst b))
                $ map (\(e1,(i1,_)) -> (i1,e1))
                $ evs1

  builderRef1 <- return . builderRef =<< get
  _ <- lift $ LLVM.buildRetVoid builderRef1

  return ()

compileSigsAux :: [(Int,Signal Double)] -> C ()
compileSigsAux es = mapM_ (uncurry go)  es
  where
  go :: Int -> Signal Double -> C ()
  go i e = do
    valueRef1 <- compileSig e
    params1 <- return . params =<< get
    compileSetVectorElement (rVecRef params1) i valueRef1

    builderRef1 <- return . builderRef =<< get

    (basicBlockRef2) <- createBasicBlock ("BB_" ++ show i)

    _ <- lift $ LLVM.buildBr builderRef1 basicBlockRef2

    lift $ LLVM.positionAtEnd builderRef1 basicBlockRef2
    modify $ \c -> c {basicBlockRef = basicBlockRef2}


listPartition :: Int -> [a] -> [[a]]
listPartition _ [] = []
listPartition n xs = (take n xs) : listPartition n (drop n xs)

compileEvents :: [Signal Bool] -> C ()
compileEvents evs = mapM_ (uncurry go) $ zip [0 .. ] evs
  where
  go :: Int -> Signal Bool -> C ()
  go i sb = do
    valueRef1 <- (compileSigBool sb >>= compileBoolToDouble)
    params1 <- return . params =<< get
    compileSetVectorElement (rVecRef params1) i valueRef1

compileSigBool :: Signal Bool -> C LLVM.ValueRef
compileSigBool sb = case sb of
  Const False -> return $ LLVM.constInt LLVM.int1Type 0 0
  Const True  -> return $ LLVM.constInt LLVM.int1Type 1 0
  PrimApp Or (Pair sb1 sb2) -> do
    valueRef1 <- compileSigBool sb1
    valueRef2 <- compileSigBool sb2
    builderRef1 <- return . builderRef =<< get
    lift $ FFI.withCString [] $ \s -> LLVM.buildOr  builderRef1 valueRef1 valueRef2 s

  PrimApp And (Pair sb1 sb2) -> do
    valueRef1 <- compileSigBool sb1
    valueRef2 <- compileSigBool sb2
    builderRef1 <- return . builderRef =<< get
    lift $ FFI.withCString [] $ \s -> LLVM.buildAnd  builderRef1 valueRef1 valueRef2 s

  PrimApp Xor (Pair sb1 sb2) -> do
    valueRef1 <- compileSigBool sb1
    valueRef2 <- compileSigBool sb2
    builderRef1 <- return . builderRef =<< get
    lift $ FFI.withCString [] $ \s -> LLVM.buildXor  builderRef1 valueRef1 valueRef2 s

  PrimApp Gt s1 -> do
    valueRef1 <- compileSig s1
    builderRef1 <- return . builderRef =<< get
    let zero = LLVM.constReal LLVM.doubleType 0.0
    lift $ FFI.withCString [] $ \s -> LLVM.buildFCmp builderRef1 2 valueRef1 zero s

  PrimApp Gte s1 -> do
    valueRef1 <- compileSig s1
    builderRef1 <- return . builderRef =<< get
    let zero = LLVM.constReal LLVM.doubleType 0.0
    lift $ FFI.withCString [] $ \s -> LLVM.buildFCmp builderRef1 3 valueRef1 zero s

  PrimApp Lt s1 -> do
    valueRef1 <- compileSig s1
    builderRef1 <- return . builderRef =<< get
    let zero = LLVM.constReal LLVM.doubleType 0.0
    lift $ FFI.withCString [] $ \s -> LLVM.buildFCmp builderRef1 4 valueRef1 zero s

  PrimApp Lte s1 -> do
    valueRef1 <- compileSig s1
    builderRef1 <- return . builderRef =<< get
    let zero = LLVM.constReal LLVM.doubleType 0.0
    lift $ FFI.withCString [] $ \s -> LLVM.buildFCmp builderRef1 5 valueRef1 zero s

  PrimApp _ _ -> $impossible

compileBoolToDouble :: LLVM.ValueRef -> C LLVM.ValueRef
compileBoolToDouble b = do
  builderRef1 <- return . builderRef =<< get
  let one      = LLVM.constReal LLVM.doubleType ( 1.0)
  let minusOne = LLVM.constReal LLVM.doubleType (-1.0)
  lift $ FFI.withCString []
       $ \s -> LLVM.buildSelect builderRef1 b one minusOne s

compileSig :: Signal Double -> C LLVM.ValueRef
compileSig e = do
  builderRef1 <- return . builderRef =<< get
  case e of
    Const d -> return $ LLVM.constReal LLVM.doubleType (realToFrac d)
    Var i1 -> do
      params1 <- return . params =<< get
      compileGetVectorElement (yVecRef params1) i1
    Time -> do
      params1 <- return . params =<< get
      return $ tRef params1
    PrimApp Der (Var i1) -> do
      params1 <- return . params =<< get
      compileGetVectorElement (ypVecRef params1) i1
    PrimApp Add (Pair e1 e2) -> do
      e1Ref <- compileSig e1
      e2Ref <- compileSig e2
      lift $ FFI.withCString []
           $ \s -> LLVM.buildFAdd builderRef1 e1Ref e2Ref s
    PrimApp Div (Pair e1 e2) -> do
      e1Ref <- compileSig e1
      e2Ref <- compileSig e2
      lift $ FFI.withCString []
           $ \s -> LLVM.buildFDiv builderRef1 e1Ref e2Ref s
    PrimApp Mul (Pair e1 e2) -> do
      e1Ref <- compileSig e1
      e2Ref <- compileSig e2
      lift $ FFI.withCString []
           $ \s -> LLVM.buildFMul builderRef1 e1Ref e2Ref s

    PrimApp Pow (Pair e1 e2) -> compileFunctionCall "pow" [e1,e2]

    PrimApp Exp   e1    -> compileFunctionCall "exp"   [e1]
    PrimApp Log   e1    -> compileFunctionCall "log"   [e1]
    PrimApp Sqrt  e1    -> compileFunctionCall "sqrt"  [e1]
    PrimApp Sin   e1    -> compileFunctionCall "sin"   [e1]
    PrimApp Cos   e1    -> compileFunctionCall "cos"   [e1]
    PrimApp Tan   e1    -> compileFunctionCall "tan"   [e1]
    PrimApp Asin  e1    -> compileFunctionCall "asin"  [e1]
    PrimApp Acos  e1    -> compileFunctionCall "acos"  [e1]
    PrimApp Atan  e1    -> compileFunctionCall "atan"  [e1]
    PrimApp Sinh  e1    -> compileFunctionCall "sinh"  [e1]
    PrimApp Cosh  e1    -> compileFunctionCall "cosh"  [e1]
    PrimApp Tanh  e1    -> compileFunctionCall "tanh"  [e1]
    PrimApp Asinh e1    -> compileFunctionCall "asinh" [e1]
    PrimApp Acosh e1    -> compileFunctionCall "acosh" [e1]
    PrimApp Atanh e1    -> compileFunctionCall "atanh" [e1]
    PrimApp Abs   e1    -> compileFunctionCall "fabs"  [e1]
    PrimApp Sgn   e1    -> compileFunctionCall "hydra_signum" [e1]

    PrimApp _ _ -> $impossible

compileFunctionCall :: String -> [Signal Double] -> C LLVM.ValueRef
compileFunctionCall s es = do
  argRefs <- mapM compileSig es
  funRef <- getFunctionRef s
  compileFunctionCallValueRef funRef argRefs

compileFunctionCallValueRef :: LLVM.ValueRef -> [LLVM.ValueRef] -> C LLVM.ValueRef
compileFunctionCallValueRef funRef argRefs = do
  builderRef1 <- return . builderRef =<< get
  lift $ FFI.withCString []
       $ \s1 -> FFI.withArray argRefs
       $ \args -> LLVM.buildCall builderRef1 funRef args (fromIntegral $ length argRefs) s1

compileGetVectorElement :: LLVM.ValueRef -> Int -> C LLVM.ValueRef
compileGetVectorElement v i = do
  builderRef1 <- return . builderRef =<< get
  ptrRef1 <- lift $ FFI.withCString []
                  $ \s1 -> FFI.withArray [LLVM.constInt LLVM.int32Type (fromIntegral i) 0]
                  $ \args -> LLVM.buildGEP builderRef1 v args 1 s1
  lift $  FFI.withCString []
       $ \s1 -> LLVM.buildLoad builderRef1 ptrRef1 s1


compileSetVectorElement :: LLVM.ValueRef -> Int -> LLVM.ValueRef -> C ()
compileSetVectorElement v i x = do
  builderRef1 <- return . builderRef =<< get
  ptrRef1 <- lift $ FFI.withCString []
                  $ \s1 -> FFI.withArray [LLVM.constInt LLVM.int32Type (fromIntegral i) 0]
                  $ \args -> LLVM.buildGEP builderRef1 v args 1 s1
  _ <- lift $ LLVM.buildStore builderRef1 x ptrRef1
  return ()

getFunctionRef :: String -> C LLVM.ValueRef
getFunctionRef s = do
  c <- get
  let modRef1 = modRef c
  functionRef1 <- lift $ FFI.withCString s $ LLVM.getNamedFunction modRef1
  if functionRef1 == FFI.nullPtr
    then fail $ "Function " ++ s ++ " does not exist"
    else return functionRef1

compileBasicBlockEntry :: C ()
compileBasicBlockEntry = do
  (basicBlockRef1) <- createBasicBlock "entry"
  builderRef1 <- return . builderRef =<< get
  lift $ LLVM.positionAtEnd builderRef1 basicBlockRef1
  modify $ \c -> c {basicBlockRef = basicBlockRef1}

createBasicBlock :: String -> C (LLVM.BasicBlockRef)
createBasicBlock s = do
  functionRef1 <- return . functionRef =<< get
  basicBlockRef1 <- lift
    $ FFI.withCString s
    $ \s1 -> LLVM.appendBasicBlock functionRef1 s1
  -- builderRef1 <- lift $ LLVM.createBuilder
  -- lift $ LLVM.positionAtEnd builderRef1 basicBlockRef1

  return (basicBlockRef1)

cleanupBuilder :: C ()
cleanupBuilder = do
  builderRef1 <- return . builderRef =<< get
  if builderRef1 == FFI.nullPtr
    then return ()
    else do modify $ \c -> c {builderRef = FFI.nullPtr}
            lift (FFI.newForeignPtr LLVM.ptrDisposeBuilder builderRef1 >>= FFI.finalizeForeignPtr)