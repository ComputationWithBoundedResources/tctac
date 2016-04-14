##tct-utils

This package provides a library to run experiments.

### Example Usage

```haskell
#!/usr/bin/env stack
-- stack --stack-yaml /home/c7031025/software/tctac/stack.yaml runghc --package tctac

import Runner
import Summer

e =  Experiment
  { eTestbed   = "/tmp/examples/"
  , eIgnore    = "/tmp/examples/"
  , eProcesses = 2
  , eTimeout   = 33
  , eTools     = [ mkTool "mat1" "matrices :from 0 :to 1", mkTool "mat2" "matrices :from 0 :to 2" ]
  }

mkTool t s = Tool
  { tName          = t
  , tExtension     = "trs"
  , tCommand       = "tct-trs"
  , tArguments     = "-t30", "-s", s
  , tProcessor     = firstline 
  } where firstline = (if null s then "MAYBE" else head xs) . lines

```
