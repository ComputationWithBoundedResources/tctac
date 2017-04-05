{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric  #-}
-- | This module provides a tool for running experiments.
module Runner
  ( Experiment (..), Tool (..), Outcome (..) , Result (..), Process
  , run
  , allLines, firstLine, termcomp, tttac, termcomp'
  ) where


import           Control.Concurrent.PooledIO.Independent (runLimited)
import           Control.Monad                           (forM_, void, when)
import           Control.Monad.Except                    (catchError)
import qualified Data.Aeson                              as A
import           Data.Function                           ((&))
import           Data.Maybe
import           Data.Monoid                             ((<>))
import           GHC.Generics                            (Generic)
import           GHC.IO.Handle
import qualified System.Directory                        as Dir
import           System.Exit                             (ExitCode (..),
                                                          exitFailure)
import           System.FilePath.Posix                   ((<.>), (</>))
import qualified System.FilePath.Posix                   as FP
import           System.Process
import qualified System.Timeout                          as T (timeout)

import           Util


-- | Specification of an experiment.
data Experiment = Experiment
  { eTestbed   :: FilePath       -- ^ path to problem testbed
  , eIgnore    :: FilePath       -- ^ problem path to ignore relative to 'eTestbed'
  , eProcesses :: Int            -- ^ number of parallel processes
  , eTimeout   :: Int            -- ^ timeout for a test in seconds
  , eTools     :: [Tool Process] -- ^ a collection of tool specifications
  , eRepeat    :: Bool           -- ^ repeat an experiment; removes tool directories specified in eTools
  }

-- | (Post-)processor for tool output.
type Process = Outcome String -> Outcome String

-- | Some useful post processors.
allLines, firstLine, termcomp, termcomp', tttac  :: Process
allLines out  = out

firstLine (Success out) = let ls = lines out in if null ls then Maybe else Success (head ls)
firstLine out           = out

termcomp out = case firstLine out of
  Success ('W':'O':'R':'S':'T':'_':'C':'A':'S':'E':'(':xs) -> Success . init . tail $ dropWhile (/= ',') xs
  Success _                                                -> Maybe
  _                                                        -> out
tttac out = case firstLine out of
  Success ('Y':'E':'S':'(': xs) -> Success . init . tail $ dropWhile (/= ',') xs
  Success _                     -> Maybe
  _                             -> out

termcomp' out = case firstLine out of
  Success ('W':'O':'R':'S':'T':'_':'C':'A':'S':'E':'(':xs) -> Success . init . tail $ takeWhile (/=  ',') xs
  Success _                                                -> Maybe
  _                                                        -> out

process :: Tool Process -> Result -> Result
process t r = r{rOutcome = tProcessor t (rOutcome r)}


-- TODO: handle arguments properly
-- | Specification of a tool.
data Tool a = Tool
  { tName      :: String   -- ^ name of tool.
  , tExtension :: String   -- ^ file extension to consider.
  , tCommand   :: String   -- ^ the command to run; in PATH, or absolute or relative path
  , tArguments :: [String] -- ^ arguments
  , tProcessor :: a        -- ^ how to process the tool output
  } deriving (Eq, Ord, Show, Functor, Generic, A.ToJSON, A.FromJSON)

type Test = (FilePath, Tool Process)

type Out     = String
type Err     = String

-- | The outcome of applying a tool on a problem.
data Outcome a = Success a | Maybe | Failure Err | Timeout
  deriving (Eq, Ord, Show, Functor, Generic, A.ToJSON, A.FromJSON)

-- TODO we don't need result; create serialisable value immediatelly
-- | The result of a test.
data Result = Result
  { rTool    :: Tool ()  -- ^ considered tool
  , rProblem :: FilePath -- ^ considered problem
  , rOutcome :: Outcome String  -- ^ outcome of applying 'rTool' to 'rProblem'.
  , rTime    :: Double   -- ^ time needed
  } deriving (Eq, Ord, Show, Generic, A.ToJSON, A.FromJSON)

ppResult :: Result -> String
ppResult Result{rTool=t, rProblem=p, rOutcome=o, rTime=z } =
  fillSep 20 (tName t)
  <> fillSep 15 (FP.takeBaseName p)
  <> fillSep 18 (show o)
  <> showDouble z
  where fillSep i s = take i (s <> repeat ' ') <> "  "

--- * go -------------------------------------------------------------------------------------------------------------

-- runGetResults :: Experiment -> IO [Result]
-- runGetResults e = do
--   when (eRepeat e) deleteTests
--   ps <- findProblems << shoutLn "find ..." << shoutLn ">>>START"
--   ts <- initTests ps << shoutLn "init ..."
--   runTests e ts      << shoutLn "run ..." <* shoutLn "<<<END"

--   `catchError` handler
--   where
--     deleteTests = forM_ (eTools e) $ \t -> removeDirectoryForcefully (tName t)
--     -- FIXME: url encode? files with # make anchors
--     findProblems = lines <$> readCreateProcess (shell $ "find " <> eTestbed e <> " -type f") mempty -- TODO: built in function
--     initTests ps =
--       sequence
--         [ copyFileIfMissing p fp >> return (fp,t)
--           | p <- ps
--           , t <- eTools e
--           , hasExtension (tExtension t) p
--           , let fp = tName t </> FP.makeRelative (eIgnore e) p ]
--     handler err = printError ("An error occured: " ++ show err ++ "\n") >> exitFailure


run :: Experiment -> IO ()
run e = do
  when (eRepeat e) deleteTests
  ps <- findProblems << shoutLn "find ..." << shoutLn ">>>START"
  ts <- initTests ps << shoutLn "init ..."
  runTests e ts      << shoutLn "run ..." <* shoutLn "<<<END"

  `catchError` handler
  where
    deleteTests = forM_ (eTools e) $ \t -> removeDirectoryForcefully (tName t)
    -- FIXME: url encode? files with # make anchors
    findProblems = lines <$> readCreateProcess (shell $ "find " <> eTestbed e <> " -type f") mempty -- TODO: built in function
    initTests ps =
      sequence
        [ copyFileIfMissing p fp >> return (fp,t)
          | p <- ps
          , t <- eTools e
          , hasExtension (tExtension t) p
          , let fp = tName t </> FP.makeRelative (eIgnore e) p ]
    handler err = printError ("An error occured: " ++ show err ++ "\n") >> exitFailure

runTests :: Experiment -> [Test] -> IO ()
runTests e mx = runLimited (eProcesses e) (runTest e `fmap` mx)

-- runTests :: Experiment -> [Test] -> IO ()
-- runTests _ [] = return ()
-- runTests e mx = mapConcurrently (runTest e) mx1 >> threadDelay 2000 >> runTests e mx2
--   where (mx1,mx2) = splitAt (eProcesses e) mx

runTest :: Experiment -> Test -> IO ()
runTest e (p,t) =
  unlessM done $ do
    f <- eval e (p,t)
    writeProof f
    -- FIXME: thread safe shoutLn
    process t f & (\g -> shoutLn (ppResult g) >> serialise resfile g)
  where
    resfile = p <.> "result"
    errfile = p <.> "err"
    outfile = p <.> "proof"

    done = Dir.doesFileExist resfile

    writeProof r = case rOutcome r of
      Success out -> writeFile outfile out
      Failure err -> writeFile errfile err
      Timeout     -> writeFile outfile "TIMEOUT"
      Maybe       -> error "Runner.runTest: an unexpected outcome"


eval :: Experiment -> Test -> IO Result
eval e (p,t) = do
  (z,r) <- timeItT $ spawn e (p,t)
  return Result{ rProblem = p', rTool = void t, rOutcome = r , rTime = z }
  where p' = FP.makeRelative (tName t) (FP.dropExtension p)

-- use bash 'timeout' command; behaves nices
-- spawn :: Experiment -> Test -> IO (Outcome Out)
-- spawn e (p,t) = do
--   -- putStrLn $ "CMD: " ++ cmd
--   let procs = proc cmd args
--   (_, mOutH, mErrH, handle) <- createProcess procs { -- std_in = NoStream
--                                                    -- ,
--                                                      std_err = CreatePipe
--                                                    , std_out = CreatePipe }
--   fin <- waitForProcess handle
--   case fin of
--     ExitFailure i -> do
--       err <- hGetContents (fromJust mErrH)
--       if i == 124
--         then return Timeout
--         else return $ Failure ("An error occured:" ++ show i ++ ':' : err)
--     ExitSuccess -> do
--         out <- hGetContents (fromJust mOutH)
--         if null out
--           then return $ Failure "empty output"
--           else return $ Success out

--   where
--     cmd  = "timeout"
--     args = [show (eTimeout e), tCommand t] ++ tArguments t ++ [p]

spawn :: Experiment -> Test -> IO (Outcome Out)
spawn e (p,t) = do
  resM <- T.timeout (eTimeout e * e6) (readProcessWithExitCode cmd args mempty)
  return $! case resM of
    Nothing             -> Timeout
    Just (ex, out, err) -> case ex of
      ExitFailure i -> Failure ("An error occured:" ++ show i ++ ':':err)
      ExitSuccess
        | null out  -> Failure "empty output"
        | otherwise -> Success out
  where
    cmd  = tCommand t
    args = tArguments t ++ [p]

