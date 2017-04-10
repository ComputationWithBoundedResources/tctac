## tct-utils

This package provides a library to run and summarise experiments.

### Example Usage

```haskell
#!/usr/bin/env stack
  -- stack --stack-yaml $TCTAC/stack.yaml runghc --package tctac

import Runner
import Summer

e =  Experiment
  { eTestbed   = "/tmp/examples/"                -- testbed; where to find the problem examples for the experiment
  , eIgnore    = "/tmp/examples/"                -- prefix ignored when writing problem id for the summary
  , eProcesses = 2                               -- run `n` experiments in parallel
  , eTimeout   = 33                              -- timeout in seconds for each execution
  , eTools     =                                 -- list of `Tool` records
    [ mkTool "mat1" "matrices :from 0 :to 1", mkTool "mat2" "matrices :from 0 :to 2" ]
  }

mkTool t s = Tool
  { tName          = t                           -- tool id; should be unique
  , tExtension     = "trs"                       -- per tool file extension
                                                 -- the file extension is ignored when writing the summary, this is useful when we want to run tools on the same 
                                                 -- problem but each tool expects a different input format
  , tCommand       = "tct-trs"                   -- executable in PATH; currently we expect that the problem filepath is given as argument to the command
  , tArguments     = words $ "-t30 -s " ++ s     -- tool specific arguments
  , tProcessor     = firstline                   -- how to process whatever is written to stdout, usually the result of the experiment
  } where firstline = \output -> if null output then "MAYBE" else head (lines output)

main :: IO ()
main = do

  -- first run the experiment; this will create a directory `tName t` for
  -- each tool defined in `etool e` and copy all problems wrt to `tExtension t`
  -- and `eTestbed e` to the directory; additionally we create files when piping stdout to
  -- $problem.out, stderr to $problem.err, and the result wrt `tProcessor t` to $problem.result

  putStrLn "run experiment"
  run e

  -- this will collect data from the given directories and write a summary in `table.html`, 

  putStrLn "sumarise experiment"
  summarise $ tname `fmap` eTools e

  -- wether a single experiment is performed only depends on wether $problem.result exists
  -- so we can easily extend the experiment by adding additional tools; or rerun the experiments for a single tool by deleting its folder
  -- behaviour is undefined when `tName t` is not unique or similar shenanigans
```
