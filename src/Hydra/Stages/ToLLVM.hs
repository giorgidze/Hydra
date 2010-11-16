{-# LANGUAGE TemplateHaskell #-}

module Hydra.Stages.ToLLVM (compile) where

import qualified Paths_Hydra as Cabal

import Hydra.Utils.Impossible (impossible)
import qualified Hydra.Utils.LLVM as LLVM (readBitcode)

import Hydra.Data

import qualified LLVM.FFI.Core as LLVM

import qualified Foreign as FFI
import qualified Foreign.C.String as FFI

import Control.Monad.State.Strict
import qualified Data.Map  as Map
import qualified Data.List as List

type C a = StateT Context IO a

data Context = Context {
    symtab        :: !SymTab
  , experiment    :: !Experiment
  , params        :: !Params
  , moduleRef     :: !LLVM.ModuleRef
  , functionRef   :: !LLVM.ValueRef
  , basicBlockRef :: !LLVM.BasicBlockRef
  , builderRef    :: !LLVM.BuilderRef
  }

data Params = Params { tRef     :: !LLVM.ValueRef
                     , yVecRef  :: !LLVM.ValueRef
                     , ypVecRef :: !LLVM.ValueRef
                     , rVecRef  :: !LLVM.ValueRef
                     } deriving (Eq,Show)

compile :: Experiment -> SymTab -> IO LLVM.ModuleRef
compile exper st = do
  let initialContext = Context { symtab = st
                               , experiment = exper
                               , params = Params FFI.nullPtr FFI.nullPtr FFI.nullPtr FFI.nullPtr
                               , moduleRef = FFI.nullPtr
                               , functionRef = FFI.nullPtr
                               , basicBlockRef = FFI.nullPtr
                               , builderRef = FFI.nullPtr
                               }
  c <- execStateT compileModuleRef initialContext
  return (moduleRef c)

compileModuleRef :: C ()
compileModuleRef = do

  bitcodeFile <- lift (Cabal.getDataFileName "cbits/hydra_llvm.bc")
  moduleRef1 <- lift (LLVM.readBitcode bitcodeFile)

  modify (\c -> c {moduleRef = moduleRef1})

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

nVectorType :: LLVM.TypeRef
nVectorType = voidPointerType

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

  , (LLVM.int32Type, "hydra_equation",         [LLVM.doubleType, nVectorType, nVectorType, nVectorType, voidPointerType])
  , (LLVM.int32Type, "hydra_event_equation",   [LLVM.doubleType, nVectorType, nVectorType, LLVM.pointerType LLVM.doubleType 0, voidPointerType])
  ]

compileFunctionDeclaration :: LLVM.TypeRef -> String -> [LLVM.TypeRef] -> C ()
compileFunctionDeclaration retType name argTypes = do
  moduleRef1 <- return . moduleRef  =<< get
  funcType <- compileFunctionType retType argTypes
  _ <- lift $ FFI.withCString name $ \s -> LLVM.addFunction moduleRef1 s funcType
  return ()

compileFunctionType :: LLVM.TypeRef -> [LLVM.TypeRef] -> C LLVM.TypeRef
compileFunctionType retType argTypes = do
  lift $ FFI.withArray argTypes
       $ \a -> return (LLVM.functionType retType a (fromIntegral $ length argTypes) 0)

compileEquation :: C ()
compileEquation = do
  functionRef1 <- getFunctionRef "hydra_equation"
  modify $ \c -> c {functionRef = functionRef1}

  compileBasicBlockEntry

  hydraNVDataRef <- getFunctionRef "hydra_nv_data"
  yVecRef1  <- compileFunctionCallValueRef hydraNVDataRef [LLVM.getParam functionRef1 1]
  ypVecRef1 <- compileFunctionCallValueRef hydraNVDataRef [LLVM.getParam functionRef1 2]
  rVecRef1  <- compileFunctionCallValueRef hydraNVDataRef [LLVM.getParam functionRef1 3]
  
  let params1 = Params { tRef     = LLVM.getParam functionRef1 0
                       , yVecRef  = yVecRef1
                       , ypVecRef = ypVecRef1
                       , rVecRef  = rVecRef1
                       }
  modify $ \c -> c {params = params1}

  symtab1 <- return . symtab  =<< get

  let exprs = equations symtab1
  -- compileSigs exprs
  compileSigsAux $ zip [0..] exprs

  builderRef1 <- return . builderRef =<< get
  _ <- lift $ LLVM.buildRet builderRef1 $ LLVM.constInt LLVM.int32Type 0 0

  return ()

compileEventEquation :: C ()
compileEventEquation = do
  functionRef1 <- getFunctionRef "hydra_event_equation"
  modify $ \c -> c {functionRef = functionRef1}

  compileBasicBlockEntry

  hydraNVDataRef <- getFunctionRef "hydra_nv_data"
  yVecRef1  <- compileFunctionCallValueRef hydraNVDataRef [LLVM.getParam functionRef1 1]
  ypVecRef1 <- compileFunctionCallValueRef hydraNVDataRef [LLVM.getParam functionRef1 2]


  let params1 = Params { tRef     = LLVM.getParam functionRef1 0
                       , yVecRef  = yVecRef1
                       , ypVecRef = ypVecRef1
                       , rVecRef  = LLVM.getParam functionRef1 3
                       }
  modify $ \c -> c {params = params1}

  evs1 <- return . events . symtab =<< get

  compileEvents $ map snd
                $ List.sort -- $ List.sortBy (\e1 e2 -> compare (fst e1) (fst e2))
                $ map (\(e1,(i1,_)) -> (i1,e1))
                $ Map.assocs evs1

  builderRef1 <- return . builderRef =<< get
  _ <- lift $ LLVM.buildRet builderRef1 $ LLVM.constInt LLVM.int32Type 0 0

  return ()

compileSigs :: [Signal Double] -> C ()
compileSigs es = mapM_ (uncurry go) $ zip [0 .. ] $ listPartition 10 $ zip [0 .. ] es
  where
  go :: Int -> [(Int,Signal Double)] -> C ()
  go i ies = do
    context <- get
    case params context of
      Params tRef1 yVecRef1 ypVecRef1 rVecRef1 -> do
        let functionName = "hydra_equation_" ++ show i
        compileFunctionDeclaration LLVM.voidType functionName [LLVM.doubleType, nVectorType, nVectorType, nVectorType]

        functionRef1 <- getFunctionRef functionName
        modify $ \c -> c {functionRef = functionRef1}

        let params1 = Params { tRef     = LLVM.getParam functionRef1 0
                             , yVecRef  = LLVM.getParam functionRef1 1
                             , ypVecRef = LLVM.getParam functionRef1 2
                             , rVecRef  = LLVM.getParam functionRef1 3
                             }
        modify $ \c -> c {params = params1}

        compileBasicBlockEntry
        -- valueRef1 <- compileSig e
        -- compileSetVectorElement (rVecRef params1) i valueRef1
        compileSigsAux ies

        builderRef1 <- return . builderRef =<< get
        _ <- lift $ LLVM.buildRetVoid builderRef1

        put context
        builderRef2 <- return . builderRef =<< get
        basicBlockRef2 <- return . basicBlockRef =<< get
        lift $ LLVM.positionAtEnd builderRef2 basicBlockRef2
        _ <- compileFunctionCallValueRef functionRef1 [tRef1, yVecRef1, ypVecRef1, rVecRef1]
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
  Or sb1 sb2 -> do
    valueRef1 <- compileSigBool sb1
    valueRef2 <- compileSigBool sb2
    builderRef1 <- return . builderRef =<< get
    lift $ FFI.withCString [] $ \s -> LLVM.buildOr  builderRef1 valueRef1 valueRef2 s

  And sb1 sb2 -> do
    valueRef1 <- compileSigBool sb1
    valueRef2 <- compileSigBool sb2
    builderRef1 <- return . builderRef =<< get
    lift $ FFI.withCString [] $ \s -> LLVM.buildAnd  builderRef1 valueRef1 valueRef2 s

  Xor sb1 sb2 -> do
    valueRef1 <- compileSigBool sb1
    valueRef2 <- compileSigBool sb2
    builderRef1 <- return . builderRef =<< get
    lift $ FFI.withCString [] $ \s -> LLVM.buildXor  builderRef1 valueRef1 valueRef2 s

  Comp f1 s1 -> do
    valueRef1 <- compileSig s1
    builderRef1 <- return . builderRef =<< get
    let zero = LLVM.constReal LLVM.doubleType 0.0
    case f1 of
      Gt  -> lift $ FFI.withCString [] $ \s -> LLVM.buildFCmp builderRef1 2 valueRef1 zero s
      Gte -> lift $ FFI.withCString [] $ \s -> LLVM.buildFCmp builderRef1 3 valueRef1 zero s
      Lt  -> lift $ FFI.withCString [] $ \s -> LLVM.buildFCmp builderRef1 4 valueRef1 zero s
      Lte -> lift $ FFI.withCString [] $ \s -> LLVM.buildFCmp builderRef1 5 valueRef1 zero s


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
    Der (Var i1) -> do
      params1 <- return . params =<< get
      compileGetVectorElement (ypVecRef params1) i1
    App2 Add e1 e2 -> do
      e1Ref <- compileSig e1
      e2Ref <- compileSig e2
      lift $ FFI.withCString []
           $ \s -> LLVM.buildFAdd builderRef1 e1Ref e2Ref s
    App2 Div e1 e2 -> do
      e1Ref <- compileSig e1
      e2Ref <- compileSig e2
      lift $ FFI.withCString []
           $ \s -> LLVM.buildFDiv builderRef1 e1Ref e2Ref s
    App2 Mul e1 e2 -> do
      e1Ref <- compileSig e1
      e2Ref <- compileSig e2
      lift $ FFI.withCString []
           $ \s -> LLVM.buildFMul builderRef1 e1Ref e2Ref s

    App2 Pow   e1 e2 -> compileFunctionCall "pow"   [e1,e2]
    App1 Exp   e1    -> compileFunctionCall "exp"   [e1]
    App1 Log   e1    -> compileFunctionCall "log"   [e1]
    App1 Sqrt  e1    -> compileFunctionCall "sqrt"  [e1]
    App1 Sin   e1    -> compileFunctionCall "sin"   [e1]
    App1 Cos   e1    -> compileFunctionCall "cos"   [e1]
    App1 Tan   e1    -> compileFunctionCall "tan"   [e1]
    App1 Asin  e1    -> compileFunctionCall "asin"  [e1]
    App1 Acos  e1    -> compileFunctionCall "acos"  [e1]
    App1 Atan  e1    -> compileFunctionCall "atan"  [e1]
    App1 Sinh  e1    -> compileFunctionCall "sinh"  [e1]
    App1 Cosh  e1    -> compileFunctionCall "cosh"  [e1]
    App1 Tanh  e1    -> compileFunctionCall "tanh"  [e1]
    App1 Asinh e1    -> compileFunctionCall "asinh" [e1]
    App1 Acosh e1    -> compileFunctionCall "acosh" [e1]
    App1 Atanh e1    -> compileFunctionCall "atanh" [e1]
    App1 Abs   e1    -> compileFunctionCall "fabs"  [e1]
    App1 Sgn   e1    -> compileFunctionCall "hydra_sgn" [e1]

    Der _   -> $impossible
    Cur _   -> $impossible


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
  let moduleRef1 = moduleRef c
  functionRef1 <- lift $ FFI.withCString s $ LLVM.getNamedFunction moduleRef1
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