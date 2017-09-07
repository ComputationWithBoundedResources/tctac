{-
This module provides support for CVS files.

The expected format is a \CSV\ table with a header. The first entry of the header is fixed to
"problem" and the delimiter is fixed to semicolon.
@
problem   ; tool1   ; ... ; toolN
test1.trs ; result1 ; ... ; resultN
...
testN.trs ; result1 ; ... ; resultN
@

USAGE:
@
fromcvs file.csv toolX problemX
@
Parses the CSV file and looks up the cell corresponding to toolX and problemX. The content of the
cell is printed to stdOut if found, otherwise "Nothing".

The intendend way to use this with \tctac\ is to define a @Tool@, for example:
@
t name = Tool
  { tName      = name
  , tExtension = "koat"
  , tCommand   =  "fromcvs"
  , tArguments = ["file.csv", name]
  , tProcessor = termcomp
}
@
This is not very efficient, but works reasonable well for small to medium benchmarks.

Known Issues:
  - The parser is white-space sensitive.
-}
import Prelude                    hiding (lookup, putStr, readFile)

import Control.Monad              (when)
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString.Char8      (ByteString, pack, putStr, readFile)
import Data.Char                  (ord)
import Data.Csv.Parser            (DecodeOptions (..), csvWithHeader, defaultDecodeOptions)
import Data.HashMap.Lazy          (lookup)
import Data.List                  (stripPrefix)
import Data.Maybe                 (fromMaybe)
import Data.Vector                (find)
import System.Environment         (getArgs)
import System.Exit                (exitFailure, exitSuccess)


main :: IO ()
main = do
  as <- getArgs
  when (null $ take 2 as) $ putStrLn "USAGE: fromcvs file.csv toolId problemId" >> exitFailure
  let file:tool:problem:_ = as
  table <- parse <$> readFile file
  case table of
    Left e  -> putStrLn e >> exitFailure
    Right f -> putStr (fromMaybe (pack "Nothing") (f tool problem)) >> exitSuccess

type Table = (String -> String -> Maybe ByteString)

parse :: ByteString -> Either String Table
parse bs = toTable <$> parseOnly parser bs
  where
  toTable (_,rows) = \tool problem ->
    let
      t = pack tool
      -- tctac does not use a global benchmark but copies all example in toolX folder; we strip toolX/ if present
      p = pack $ fromMaybe problem $ stripPrefix (tool ++ "/") problem
    in
    find ((Just p ==)  . lookup (pack "problem")) rows >>= lookup t
  parser    = csvWithHeader $ defaultDecodeOptions { decDelimiter = delimiter }
  delimiter = fromIntegral (ord ';')

