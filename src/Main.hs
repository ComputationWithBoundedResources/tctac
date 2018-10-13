

module Main where
-- stack --stack-yaml $TCTAC/stack.yaml runghc --package tctac

import           Runner
import           Summer

timeout :: Int
timeout = 60

e :: Experiment
e =  Experiment
  {
  --   eTestbed   = "/home/schnecki/Documents/Jobs/UIBK/CL/amortized-resource-analysis/inference/doc/tpdb_full/"
  -- , eIgnore    = "/home/schnecki/Documents/Jobs/UIBK/CL/amortized-resource-analysis/inference/doc/tpdb_full/"
    eTestbed   = "/home/schnecki/Documents/projects/ara/ara/doc/fp"
  , eIgnore    = "/home/schnecki/Documents/projects/ara/ara/doc/fp"
  --   eTestbed   = "/home/schnecki/Documents/Jobs/UIBK/CL/amortized-resource-analysis/inference/doc/examples/"
  -- , eIgnore    = "/home/schnecki/Documents/Jobs/UIBK/CL/amortized-resource-analysis/inference/doc/examples/"
  , eProcesses = 1
  , eTimeout   = timeout
  , eTools     = [--  mkToolTct "Mat" "Cmatrices"
                 -- , mkToolTct "Poly" "Cpolys"
                 -- , mkToolTct "Ints" "Cints"
                 -- mkToolTct "CompAraParallel" "competition"
                 -- mkToolTct "CompAra" "competition"


                 --   mkToolAraHoca "AraHocaV1" ["-v1", "-m1"]
                 -- , mkToolAraHoca "AraHocaV2" ["-v2", "-m2"]
                 -- , mkToolAraHoca "AraHocaV3" ["-v3", "-m3"]
                 -- , mkToolAraHoca "AraHocaV1Heur" ["-v1", "-m1", "-b"]
                 -- , mkToolAraHoca "AraHocaV2Heur" ["-v2", "-m2", "-b"]
                 -- , mkToolAraHoca "AraHocaV3Heur" ["-v3", "-m3", "-b"]

                 -- , mkToolAraHoca "AraHocaV1BestCase" ["-v1", "-m1", "-l", "-n"]
                 -- , mkToolAraHoca "AraHocaV2BestCase" ["-v2", "-m2", "-l", "-n"]
                 -- , mkToolAraHoca "AraHocaV1HeurBestCase" ["-v1", "-m1", "-b", "-l", "-n"]
                 -- , mkToolAraHoca "AraHocaV2HeurBestCase" ["-v2", "-m2", "-b", "-l", "-n"]
                 -- , mkToolAraHoca "AraHocaV3HeurBestCase" ["-v3", "-m3", "-b", "-l", "-n"]

                   mkToolTct "TcT" "competition"
                 , mkToolAra "AraTRSV3BestCase" ["-v3"]
                 , mkToolAraHoca "AraHocaV3BestCase" ["-v3"]
                 , mkToolRamlLower "Raml-1.4.1" []

                 -- , mkToolAra "AraWorstCaseV3" ["-v3", "-s z3"]

                 -- -- -- mkToolTct "TctAraZ3" "ara"
                 -- , mkToolAra "AraBestCaseTraceV3Heur" ["-v3", "-l", "-b"]
                 -- , mkToolAra "AraBestCaseTraceV2Heur" ["-v2", "-l", "-b"]
                 -- , mkToolAra "AraBestCaseTraceV1Heur" ["-v1", "-l", "-b"]
                 -- , mkToolAra "AraBestCaseTraceV3" ["-v3", "-l"]
                 -- , mkToolAra "AraBestCaseTraceV2" ["-v2", "-l"]
                 -- , mkToolAra "AraBestCaseTraceV1" ["-v1", "-l"]

                 -- , mkToolAra "AraBestCaseTraceV3CFHeur" ["-v3", "-l", "-b", "-a"]
                 -- , mkToolAra "AraBestCaseTraceV2CFHeur" ["-v2", "-l", "-b", "-a"]
                 -- , mkToolAra "AraBestCaseTraceV1CFHeur" ["-v1", "-l", "-b", "-a"]
                 -- , mkToolAra "AraBestCaseTraceV3CF" ["-v3", "-l", "-a"]
                 -- , mkToolAra "AraBestCaseTraceV2CF" ["-v2", "-l", "-a"]
                 -- , mkToolAra "AraBestCaseTraceV1CF" ["-v1", "-l", "-a"]

                 -- , mkToolAra "AraBestCaseTraceV3NoCdCFHeur" ["-v3", "-l", "-b", "-a", "-n"]
                 -- , mkToolAra "AraBestCaseTraceV2NoCdCFHeur" ["-v2", "-l", "-b", "-a", "-n"]
                 -- , mkToolAra "AraBestCaseTraceV1NoCdCFHeur" ["-v1", "-l", "-b", "-a", "-n"]
                 -- , mkToolAra "AraBestCaseTraceV3NoCdCF" ["-v3", "-l", "-a", "-n"]
                 -- , mkToolAra "AraBestCaseTraceV2NoCdCF" ["-v2", "-l", "-a", "-n"]
                 -- , mkToolAra "AraBestCaseTraceV1NoCdCF" ["-v1", "-l", "-a", "-n"]
                 -- , mkToolAra "AraBestCaseTraceV3NoCdHeur" ["-v3", "-l", "-b", "-n"]
                 -- , mkToolAra "AraBestCaseTraceV2NoCdHeur" ["-v2", "-l", "-b", "-n"]
                 -- , mkToolAra "AraBestCaseTraceV1NoCdHeur" ["-v1", "-l", "-b", "-n"]

                 -- , mkToolAra "AraBestCaseTraceV3NoCd" ["-v3", "-l", "-n"]
                 -- , mkToolAra "AraBestCaseTraceV2NoCd" ["-v2", "-l", "-n"]
                 -- , mkToolAra "AraBestCaseTraceV1NoCd" ["-v1", "-l", "-n"]


                 -- , mkToolAra "AraBestCaseSize" ["-v1", "--lowerbound"]
                 -- , mkToolAra "AraBestCaseSizeNoCd" ["-v1", "--lowerbound", "-n"]

                  -- mkToolAra "CompletelyDefined" ["-l"]

                 -- , mkToolAra "AraMinismt" ["-v3", "-s minismt"]
                 -- , mkToolAra "AraHeurZ3" ["-b", "-v3", "-s z3"]
                 -- , mkToolAra "AraHeurMinismt" ["-b", "-v3", "-s minismt"]
                 -- , mkToolAra "AraSCC" ["-c", "-v 3"]
                 -- , mkToolAra "AraSCCHeur" ["-c", "-b", "-v 3"]
                 ]
  , eRepeat = False
  }

mkToolTct :: String -> String -> Tool Process
mkToolTct t s = Tool
  { tName          = t
  , tExtension     = "raml.trs"
  , tCommand       = "tct-trs"
  , tArguments     = ["--complexity", "rci","-s", s, "-t", show timeout]
  , tProcessor     = firstLine
  }

mkToolAra :: String -> [String] -> Tool Process
mkToolAra t s = Tool
  { tName          = t
  , tExtension     = "raml.trs"
  , tCommand       = "ara-trs"
  , tArguments     = ("-t " ++ show timeout) : s
  , tProcessor     = firstLine
  }

mkToolAraHoca :: String -> [String] -> Tool Process
mkToolAraHoca t s = Tool
  { tName          = t
  , tExtension     = "raml.fp"
  , tCommand       = "ara-hoca"
  , tArguments     = ("-t " ++ show timeout) : s
  , tProcessor     = firstLine
  }

mkToolRamlLower :: String -> [String] -> Tool Process
mkToolRamlLower t s = Tool
  { tName          = t
  , tExtension     = "raml.raml"
  , tCommand       = "raml"
  , tArguments     = ["analyze", "upper", "steps", "1", "3", "-m", "-print", "level", "2" ] ++ s
  , tProcessor     = allLines
  }


main :: IO ()
main = do
  run e
  summarise $ map tName (eTools e) -- summarise all registered tools
           -- ++ [ "AraSCCHeur" ]  -- plus possibility some others
  return ()
