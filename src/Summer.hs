{-# LANGUAGE QuasiQuotes #-}
module Summer where

import qualified Data.Map.Strict    as M
import qualified Data.Set           as S
import Data.Monoid
import           System.Process
import           Control.Exception  (bracket)
import           System.Directory
-- import           System.Environment (getArgs)
import           System.FilePath
import           Text.CSV           (parseCSVFromFile)

import Text.Hamlet
import Text.Blaze.Html.Renderer.String (renderHtml)

-- import Debug.Trace

-- import Turtle

synopsis :: String
synopsis = "sumex [--dir Filepath] [--tools tool1 tool2 ...]"



dostuff :: IO()
dostuff = gather "mat1"
  

gather :: String -> IO ()
gather s = callCommand $
  "for f in $(find " <> s <> " -type f -name '*.result'); do (cat \"${f}\"; echo) >> " <> s <> ".summary.csv; done"

-- main :: IO ()
-- main = do
--   (fp:ts) <- getArgs
--   readAndWrite fp ts

type Problem = FilePath
type Tool    = FilePath
type Answer  = String
type Time    = String

data Result = Result Tool (M.Map Problem (Maybe (Problem,Answer,Time)))
  deriving Show

readAndWrite :: FilePath -> [Tool] -> IO ()
readAndWrite fp ts = withDir fp $ readResults ts >>= \rs -> writeResults (prepareResults rs)
  where withDir dir io = bracket getCurrentDirectory setCurrentDirectory $ \ _ -> setCurrentDirectory dir >> io

readResults :: [Tool] -> IO [Result]
readResults ts = readResult `mapM` ts

readResult :: Tool -> IO Result
readResult t = do
  let
    fp = t<>".summary.csv"
    fromCSV [tool,prob, res, time] es = (dropExtension prob, Just (t</>prob,res,time)):es
    fromCSV [""] es              = es
    fromCSV rs es                = error $ "An error occured when processing the csv file:" ++ fp ++ ":" ++ show rs

  csvE <- parseCSVFromFile fp
  case csvE of
    Left  err -> error $ "An error occured when parsing the csv file:" ++ fp ++ ":" ++ show err
    Right csv -> return (Result t (M.fromList $ foldr fromCSV [] csv))

prepareResults :: [Result] -> [Result]
prepareResults rs = fillMissing  `fmap` rs
  where
    allps = S.unions $ (\(Result _ m) -> M.keysSet m) `fmap` rs
    dummy = M.fromList $ zip (S.toList allps) (repeat Nothing)
    fillMissing (Result t m) = Result t (M.union m dummy)

mkTable :: [Result] -> ([Tool],[(Problem,[Maybe(Problem,Answer,Time)])])
mkTable rs = M.assocs `fmap` foldr merge ([],M.empty) rs
  where
    merge (Result t m) (ts,ms) = (t:ts, merge' m ms)
    merge'                     = M.mergeWithKey (\k a b -> Just (a:b)) (fmap (:[])) id

writeResults :: [Result] -> IO ()
writeResults = writeTable

writeTable :: [Result] -> IO()
writeTable rs = do
  let (ts,es) = mkTable rs
  putStrLn $ renderHtml $
    [shamlet|
$doctype 5
<html>
  <head>
    <title> experiment
  <body>
    <table>
      <tr>
        <td>
        $forall t <- ts
          <td>#{t}
      $forall (p,rs) <- es
        <tr>
          $forall mr <- rs
            <td>#{p}
              $maybe (a,b,c) <- mr
                <td>#{b}
              $nothing
                <td> missing
    |]

