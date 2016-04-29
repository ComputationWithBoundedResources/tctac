module RunAndSum where

import Runner
import Summer

import Options.Applicative

import Control.Monad (join)

opts :: Parser (IO ())
opts = subparser
  ( command "run"  (info (start <$> argument str idm) idm)
 <> command "sum"  (info (pure stop) idm) )

tctac :: IO ()
tctac = join $ execParser (info opts idm)



start :: String -> IO ()
start = undefined
stop :: IO ()
stop = undefined
