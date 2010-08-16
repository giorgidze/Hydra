module Main where

import System
import Text.Regex

substs :: [(Regex,String)]
substs = map (\(r, s) -> (mkRegex r, s)) [
    ("module[[:space:]]+AbsHydra[[:space:]]+where"    , "module Hydra.BNFC.AbsHydra where")
  , ("import[[:space:]]+AbsHydra"                     , "import Hydra.BNFC.AbsHydra")
  , ("module[[:space:]]+LexHydra[[:space:]]+where"    , "module Hydra.BNFC.LexHydra where")
  , ("import[[:space:]]+LexHydra"                     , "import Hydra.BNFC.LexHydra")
  , ("module[[:space:]]+ParHydra[[:space:]]+where"    , "module Hydra.BNFC.ParHydra where")
  , ("import[[:space:]]+ParHydra"                     , "import Hydra.BNFC.ParHydra")
  , ("module[[:space:]]+ErrM[[:space:]]+where"        , "module Hydra.BNFC.ErrM where")
  , ("import[[:space:]]+ErrM"                         , "import Hydra.BNFC.ErrM")
  , ("module[[:space:]]+LayoutHydra[[:space:]]+where" , "module Hydra.BNFC.LayoutHydra where")
  , ("import[[:space:]]+LayoutHydra"                  , "import Hydra.BNFC.LayoutHydra")
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      inputFileContent <- readFile inputFile
      let pp = (\(r,s') s -> subRegex r s s')
          header = "{-# OPTIONS -w #-}\n"
          outputFileContent = header ++ foldr pp inputFileContent substs
      writeFile outputFile outputFileContent
    _ -> fail "usage: runhaskell Preprocessor.hs inputFile outputFile"
