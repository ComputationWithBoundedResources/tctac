{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Util where


import           Control.Exception     (bracket)
import           Control.Monad         (when)
import           Control.Monad.Except  (unless)
import qualified Data.Aeson            as A
import qualified Data.ByteString.Lazy  as BS (readFile, writeFile)
import           Data.Maybe            (fromMaybe)
import qualified System.Directory      as Dir
import qualified System.FilePath.Posix as FP
import           System.IO             (hPutStr, stderr)
import           System.Time           (ClockTime (TOD), getClockTime)
import           Text.Printf           (printf)


(<<) :: IO b -> IO a -> IO b
(<<) = flip (>>)

serialise :: A.ToJSON a => FilePath -> a -> IO ()
serialise fp = BS.writeFile fp . A.encode

deserialise :: A.FromJSON a => FilePath -> IO a
deserialise fp = (fromMaybe err . A.decode) <$> BS.readFile fp
  where err = error $ "oh no: an error occured in deserialise: " ++ fp

showDouble :: Double -> String
showDouble = printf "%.2f"

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b m = b >>= flip unless m

-- | > hasExtension "trs" "problem.trs = True
-- | > hasExtension ".trs" "problem.trs = True
hasExtension :: String -> FilePath -> Bool
hasExtension s fp = s == ex || s == tail ex
  where ex = FP.takeExtension fp

removeDirectoryForcefully :: FilePath -> IO ()
removeDirectoryForcefully fp = Dir.doesDirectoryExist fp >>= \b -> when b (Dir.removeDirectoryRecursive fp)

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action =
  bracket Dir.getCurrentDirectory Dir.setCurrentDirectory $ \ _ -> do
    Dir.setCurrentDirectory dir
    action

copyFileIfMissing :: FilePath -> FilePath -> IO ()
copyFileIfMissing fp1 fp2 = unlessM (Dir.doesFileExist fp2) $
  Dir.createDirectoryIfMissing True (FP.takeDirectory fp2) >> Dir.copyFile fp1 fp2

shout :: String -> IO ()
shout = putStr

shoutLn :: String -> IO ()
shoutLn = putStrLn

e6 :: Num i => i
e6 = 10^6

-- MS: System.TimeIt and System.getCPUTime do not work for me for some unknown reason.
timeItT :: IO a -> IO (Double, a)
timeItT ioa = do
  t1 <- getClockTimeMS
  a  <- ioa >>= \a -> a `seq` return a
  t2 <- getClockTimeMS
  let t = fromIntegral (t2-t1) / e6
  return (t,a)
  where
    getClockTimeMS = do
      (TOD s p) <- getClockTime
      return $ fromIntegral (s * e6 + p `div` e6)

printError :: String -> IO ()
printError = hPutStr stderr

