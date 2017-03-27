

module Main where
-- stack --stack-yaml $TCTAC/stack.yaml runghc --package tctac

import           Runner
import           Summer

e =  Experiment
  {
  --   eTestbed   = "/home/schnecki/Documents/Jobs/UIBK/CL/amortized-resource-analysis/inference/doc/tpdb_full/"
  -- , eIgnore    = "/home/schnecki/Documents/Jobs/UIBK/CL/amortized-resource-analysis/inference/doc/tpdb_full/"
    eTestbed   = "/home/schnecki/Documents/Jobs/UIBK/CL/amortized-resource-analysis/inference/doc/tpdb_constrtrs/"
  , eIgnore    = "/home/schnecki/Documents/Jobs/UIBK/CL/amortized-resource-analysis/inference/doc/tpdb_constrtrs/"
  --   eTestbed   = "/home/schnecki/Documents/Jobs/UIBK/CL/amortized-resource-analysis/inference/doc/examples/"
  -- , eIgnore    = "/home/schnecki/Documents/Jobs/UIBK/CL/amortized-resource-analysis/inference/doc/examples/"
  , eProcesses = 2
  , eTimeout   = 60
  , eTools     = [ mkToolTct "Mat" "Cmatrices"
                 , mkToolTct "Poly" "Cpolys"
                 , mkToolTct "Ints" "Cints"
                 , mkToolTct "Comp" "competition"
                 , mkToolAra "Ara" ["-v3"]
                 , mkToolAra "AraHeur" ["-b", "-v3"]
                 -- , mkToolAra "AraSCC" ["-c", "-v 3"]
                 -- , mkToolAra "AraSCCHeur" ["-c", "-b", "-v 3"]
                 ]
  , eRepeat = True
  }

mkToolTct t s = Tool
  { tName          = t
  , tExtension     = "trs"
  , tCommand       = "tct-trs"
  , tArguments     = ["--complexity", "rci","-s", s]
  , tProcessor     = firstLine
  }

mkToolAra t s = Tool
  { tName          = t
  , tExtension     = "trs"
  , tCommand       = "inference-exe"
  , tArguments     = s
  , tProcessor     = firstLine
  }

main :: IO ()
main = do
  run e
  summarise [ "Mat"
            , "Poly"
            , "Ints"
            , "Comp"
            , "Ara"
            , "AraHeur"
            -- , "AraSCC"
            -- , "AraSCCHeur"
            ]
  return ()
