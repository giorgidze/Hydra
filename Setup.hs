import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Verbosity
import Distribution.System

import System.Directory
import Data.Char

main :: IO ()
main = defaultMainWithHooks hydraHooks

hydraHooks :: UserHooks
hydraHooks = simpleUserHooks {
    preConf  = (\args flags -> hydraPreConf  >> (preConf  simpleUserHooks) args flags)
  , preClean = (\args flags -> hydraPreClean >> (preClean simpleUserHooks) args flags)
  }


hydraPreConf :: IO ()
hydraPreConf = do
  let clang = simpleProgram "clang"
  let llvm_as = simpleProgram "llvm-as"

  conf <- configureAllKnownPrograms silent (addKnownPrograms [clang,llvm_as] defaultProgramConfiguration)

  -- requireProgram verbose clang AnyVersion conf
  -- requireProgram verbose llvm_as AnyVersion conf

  let arch = map toLower (show buildArch)

  rawSystemProgramConf verbose clang   conf ["-arch",arch,"-Wall","-O3","-S","-emit-llvm", "-o","cbits/hydra_llvm.s","cbits/hydra_llvm.c","-I/opt/local/include"]
  rawSystemProgramConf verbose llvm_as conf ["-f","-o", "cbits/hydra_llvm.bc","cbits/hydra_llvm.s"]

hydraPreClean :: IO ()
hydraPreClean = do
  removeFileSafe "cbits/hydra_llvm.bc"
  removeFileSafe "cbits/hydra_llvm.s"

removeFileSafe :: FilePath -> IO ()
removeFileSafe s = do
  b <- doesFileExist s
  if b
    then removeFile s
    else return ()