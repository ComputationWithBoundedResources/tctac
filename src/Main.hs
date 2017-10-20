

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
    eTestbed   = "../ara-inference/doc/testbed/"
  , eIgnore    = "../ara-inference/doc/testbed/"
  --   eTestbed   = "/home/schnecki/Documents/Jobs/UIBK/CL/amortized-resource-analysis/inference/doc/examples/"
  -- , eIgnore    = "/home/schnecki/Documents/Jobs/UIBK/CL/amortized-resource-analysis/inference/doc/examples/"
  , eProcesses = 2
  , eTimeout   = timeout
  , eTools     = [--  mkToolTct "Mat" "Cmatrices"
                 -- , mkToolTct "Poly" "Cpolys"
                 -- , mkToolTct "Ints" "Cints"
                 -- mkToolTct "CompAraParallel" "competition"
                 -- mkToolTct "CompAra" "competition"


                   mkToolTct "Comp" "competition"
                 , mkToolAra "AraWorstCaseV3" ["-v3", "-s z3"]

                 -- -- -- mkToolTct "TctAraZ3" "ara"
                 , mkToolAra "AraBestCaseTraceV3Heur" ["-v3", "-l", "-b"]
                 , mkToolAra "AraBestCaseTraceV2Heur" ["-v2", "-l", "-b"]
                 , mkToolAra "AraBestCaseTraceV1Heur" ["-v1", "-l", "-b"]
                 , mkToolAra "AraBestCaseTraceV3" ["-v3", "-l"]
                 , mkToolAra "AraBestCaseTraceV2" ["-v2", "-l"]
                 , mkToolAra "AraBestCaseTraceV1" ["-v1", "-l"]

                 , mkToolAra "AraBestCaseTraceV3CFHeur" ["-v3", "-l", "-b", "-a"]
                 , mkToolAra "AraBestCaseTraceV2CFHeur" ["-v2", "-l", "-b", "-a"]
                 , mkToolAra "AraBestCaseTraceV1CFHeur" ["-v1", "-l", "-b", "-a"]
                 , mkToolAra "AraBestCaseTraceV3CF" ["-v3", "-l", "-a"]
                 , mkToolAra "AraBestCaseTraceV2CF" ["-v2", "-l", "-a"]
                 , mkToolAra "AraBestCaseTraceV1CF" ["-v1", "-l", "-a"]

                 , mkToolAra "AraBestCaseTraceV3NoCdCFHeur" ["-v3", "-l", "-b", "-a", "-n"]
                 , mkToolAra "AraBestCaseTraceV2NoCdCFHeur" ["-v2", "-l", "-b", "-a", "-n"]
                 , mkToolAra "AraBestCaseTraceV1NoCdCFHeur" ["-v1", "-l", "-b", "-a", "-n"]
                 , mkToolAra "AraBestCaseTraceV3NoCdCF" ["-v3", "-l", "-a", "-n"]
                 , mkToolAra "AraBestCaseTraceV2NoCdCF" ["-v2", "-l", "-a", "-n"]
                 , mkToolAra "AraBestCaseTraceV1NoCdCF" ["-v1", "-l", "-a", "-n"]
                 , mkToolAra "AraBestCaseTraceV3NoCdHeur" ["-v3", "-l", "-b", "-n"]
                 , mkToolAra "AraBestCaseTraceV2NoCdHeur" ["-v2", "-l", "-b", "-n"]
                 , mkToolAra "AraBestCaseTraceV1NoCdHeur" ["-v1", "-l", "-b", "-n"]

                 , mkToolAra "AraBestCaseTraceV3NoCd" ["-v3", "-l"]
                 , mkToolAra "AraBestCaseTraceV2NoCd" ["-v2", "-l"]
                 , mkToolAra "AraBestCaseTraceV1NoCd" ["-v1", "-l"]


                 , mkToolAra "AraBestCaseSize" ["-v1", "--lowerbound"]

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
  , tExtension     = "trs"
  , tCommand       = "tct-trs"
  , tArguments     = ["--complexity", "rci","-s", s, "-t", show timeout]
  , tProcessor     = firstLine
  }

mkToolAra :: String -> [String] -> Tool Process
mkToolAra t s = Tool
  { tName          = t
  , tExtension     = "trs"
  , tCommand       = "ara-inference-exe"
  , tArguments     = ("-t " ++ show timeout) : s
  , tProcessor     = firstLine
  }

main :: IO ()
main = do
  run e
  summarise [ -- "Mat" -- sum
            -- , "Poly" -- sum
            -- , "Ints" -- sum
    "Comp" -- sum
    -- "CompAra" -- sum
    -- "CompAraParallel" -- sum
            -- , "Ara" -- sum
            -- , "AraHeur" -- sum
    , "AraWorstCaseV3"
    , "AraBestCaseTraceV3"
    , "AraBestCaseTraceV2"
    , "AraBestCaseTraceV1"
    , "AraBestCaseTraceV3CF"
    , "AraBestCaseTraceV2CF"
    , "AraBestCaseTraceV1CF"
    , "AraBestCaseTraceV3Heur"
    , "AraBestCaseTraceV2Heur"
    , "AraBestCaseTraceV1Heur"
    , "AraBestCaseTraceV3CFHeur"
    , "AraBestCaseTraceV2CFHeur"
    , "AraBestCaseTraceV1CFHeur"
    , "AraBestCaseTraceV3NoCd"
    , "AraBestCaseTraceV2NoCd"
    , "AraBestCaseTraceV1NoCd"
    , "AraBestCaseTraceV3NoCdHeur"
    , "AraBestCaseTraceV2NoCdHeur"
    , "AraBestCaseTraceV1NoCdHeur"
    , "AraBestCaseTraceV3NoCdCF"
    , "AraBestCaseTraceV2NoCdCF"
    , "AraBestCaseTraceV1NoCdCF"
    , "AraBestCaseTraceV3NoCdCFHeur"
    , "AraBestCaseTraceV2NoCdCFHeur"
    , "AraBestCaseTraceV1NoCdCFHeur"
    , "AraBestCaseSize"
    , "CompletelyDefined"
               -- "TctAraZ3" -- sum
          --   "AraZ3" -- sum
          -- , "AraMinismt" -- sum
          -- , "AraHeurZ3" -- sum
          -- , "AraHeurMinismt" -- sum
            -- , "AraSCC" -- sum
            -- , "AraSCCHeur" -- sum
            ]
  return ()
