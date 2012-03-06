import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Verbosity

main :: IO ()
main = defaultMainWithHooks hooks

hooks :: UserHooks
hooks = simpleUserHooks {
  confHook = confHook'
  }

confHook' :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
confHook' param cfs = do
    let sundials_config = simpleProgram "sundials-config"
    conf     <- configureAllKnownPrograms silent (addKnownPrograms [sundials_config] defaultProgramConfiguration)
    cppFlags <- rawSystemProgramStdoutConf verbose sundials_config conf ["-m","ida","-t","s","-l","c","-s","cppflags"]
    libs     <- rawSystemProgramStdoutConf verbose sundials_config conf ["-m","ida","-t","s","-l","c","-s","libs"]
    let cfs' = cfs { configExtraLibDirs      = configExtraLibDirs     cfs ++ extraLDs (words libs)
                   , configExtraIncludeDirs  = configExtraIncludeDirs cfs ++ extraIDs (words cppFlags)
                   }
    (confHook simpleUserHooks) param cfs'

extraLDs :: [String] -> [String]
extraLDs []                       = []
extraLDs (('-' : 'L' : d) : ss)   = d : extraLDs ss
extraLDs (_ : ss)                 = extraLDs ss

extraIDs :: [String] -> [String]
extraIDs []                       = []
extraIDs (('-' : 'I' : d) : ss)   = d : extraIDs ss
extraIDs (_ : ss)                 = extraIDs ss