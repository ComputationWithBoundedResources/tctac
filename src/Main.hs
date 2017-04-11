

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
    eTestbed   = "../ara-inference/doc/tpdb_constrtrs/"
  , eIgnore    = "../ara-inference/doc/tpdb_constrtrs/"
  --   eTestbed   = "/home/schnecki/Documents/Jobs/UIBK/CL/amortized-resource-analysis/inference/doc/examples/"
  -- , eIgnore    = "/home/schnecki/Documents/Jobs/UIBK/CL/amortized-resource-analysis/inference/doc/examples/"
  , eProcesses = 4
  , eTimeout   = timeout
  , eTools     = [ mkToolTct "Mat" "Cmatrices"
                 , mkToolTct "Poly" "Cpolys"
                 , mkToolTct "Ints" "Cints"
                 -- , mkToolTct "Comp" "competition"
                 , mkToolTct "Ara" "Cara"
                 , mkToolAra "AraHeur" ["-b", "-v3"]
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
  , tArguments     = ["--complexity", "rci","-s", s, "-t", show (timeout + 3)]
  , tProcessor     = firstLine
  }

mkToolAra :: String -> [String] -> Tool Process
mkToolAra t s = Tool
  { tName          = t
  , tExtension     = "trs"
  , tCommand       = "ara-inference-exe"
  , tArguments     = s
  , tProcessor     = firstLine
  }

main :: IO ()
main = do
  run e
  summarise [ "Mat"
            , "Poly"
            , "Ints"
            -- , "Comp"
            , "Ara"
            , "AraHeur"
            -- , "AraSCC"
            -- , "AraSCCHeur"
            ]
  return ()
