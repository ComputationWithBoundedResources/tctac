module RunAndSum where

import Runner
import Summer

tctac :: Experiment -> IO ()
tctac e = run e >> summarise []

