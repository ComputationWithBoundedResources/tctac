

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
    eTestbed   = "../ara-inference/doc/tpdb_full/"
  , eIgnore    = "../ara-inference/doc/tpdb_full/"
  --   eTestbed   = "/home/schnecki/Documents/Jobs/UIBK/CL/amortized-resource-analysis/inference/doc/examples/"
  -- , eIgnore    = "/home/schnecki/Documents/Jobs/UIBK/CL/amortized-resource-analysis/inference/doc/examples/"
  , eProcesses = 2
  , eTimeout   = timeout
  , eTools     = [--  mkToolTct "Mat" "Cmatrices"
                 -- , mkToolTct "Poly" "Cpolys"
                 -- , mkToolTct "Ints" "Cints"

                  mkToolTct "CompAraParallel" "competition"
                 -- mkToolTct "CompAra" "competition"
                 -- mkToolTct "Comp" "competition"

                 -- mkToolTct "TctAraZ3" "ara"
                 --   mkToolAra "AraZ3" ["-v3", "-s z3"]
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
  summarise [ -- "Mat"
            -- , "Poly"
            -- , "Ints"
    -- "Comp"
    -- "CompAra"
    "CompAraParallel"
            -- , "Ara"
            -- , "AraHeur"
               -- "TctAraZ3"
          --   "AraZ3"
          -- , "AraMinismt"
          -- , "AraHeurZ3"
          -- , "AraHeurMinismt"

            -- , "AraSCC"
            -- , "AraSCCHeur"
            ]
  return ()
