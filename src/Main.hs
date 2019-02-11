
module Main where
-- stack --stack-yaml $TCTAC/stack.yaml runghc --package tctac

import           Runner
import           Summer

timeout :: Int
timeout = 60

e :: Experiment
e =  Experiment
  {
    eTestbed   = "/home/schnecki/Documents/projects/ara/ara/doc/fp"
  , eIgnore    = "/home/schnecki/Documents/projects/ara/ara/doc/fp"
  , eProcesses = 1
  , eTimeout   = timeout
  , eTools     = [
                   mkToolTct "TcT WorstCase" "competition"
                 , mkToolAra "AraTRS-Cor29" ["-v3", "-l", "-n"]
                 , mkToolAraHoca "AraHOCA-Cor29" ["-v3", "-l", "-n"]
                 , mkToolCostabsCoflocoBC "term_depth" "Costabs-CoFloCo-TermDepth" []
                 , mkToolCostabsCoflocoBC "typed_norms" "Costabs-CoFloCo-TypedNorms" []
                 -- , mkToolRamlLower "Raml-1.4.1" []

                 ]
  , eRepeat = False
  }

mkToolTct :: String -> String -> Tool Process
mkToolTct t s = Tool
  { tName          = t
  , tExtension     = "trs"
  , tCommand       = "tct-trs"
  , tArguments     = ["--complexity", "rci","-s", s, "-t", show timeout]
  , tProcessor     = firstLine
  }

mkToolAra :: String -> [String] -> Tool Process
mkToolAra t s = Tool
  { tName          = t
  , tExtension     = "trs"
  , tCommand       = "ara-trs"
  , tArguments     = ("-t " ++ show timeout) : s
  , tProcessor     = firstLine
  }

mkToolAraHoca :: String -> [String] -> Tool Process
mkToolAraHoca t s = Tool
  { tName          = t
  , tExtension     = "fp"
  , tCommand       = "ara-hoca"
  , tArguments     = ("-t " ++ show timeout) : s
  , tProcessor     = firstLine
  }

mkToolRamlLower :: String -> [String] -> Tool Process
mkToolRamlLower t s = Tool
  { tName          = t
  , tExtension     = "raml"
  , tCommand       = "raml"
  , tArguments     = ["analyze", "upper", "steps", "1", "3", "-m", "-print", "level", "2" ] ++ s
  , tProcessor     = allLines
  }


mkToolCostabsCoflocoBC :: String -> String -> [String] -> Tool Process
mkToolCostabsCoflocoBC sizeAbst t s = Tool
  { tName          = t
  , tExtension     = "abs"
  , tCommand       = "costabsCoflocoBC"
  , tArguments     = ["-entries", "start", "-asymp", "yes", "-size_abst", sizeAbst, "-cost_model", "steps"] ++ s
  , tProcessor     = findLine (\line -> take (length str) line == str)
  }
  where str = "Possible lower bounds :"


mkToolCostabsPubsBC :: String -> String -> [String] -> Tool Process
mkToolCostabsPubsBC sizeAbst t s = Tool
  { tName          = t
  , tExtension     = "abs"
  , tCommand       = "costabsPubsBC"
  , tArguments     = ["-entries", "start", "-asymp", "yes", "-size_abst", sizeAbst, "-cost_model", "steps"] ++ s
  , tProcessor     = allLines
  }


main :: IO ()
main = do
  run e
  summarise $ map tName (eTools e) -- summarise all registered tools
           -- ++ [ "AraSCCHeur" ]  -- plus possibility some others
  return ()
