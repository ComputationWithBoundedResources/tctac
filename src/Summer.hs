{-# LANGUAGE QuasiQuotes #-}
module Summer where

import           Control.Monad                   (filterM)
import qualified Data.Map.Strict                 as M
import           Data.Monoid
import qualified Data.Set                        as S
import qualified System.Directory                as Dir
import           System.Process
-- import           System.Environment (getArgs)

import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Hamlet

import           Runner
import           Util

import Debug.Trace


synopsis :: String
synopsis = "sumex [--tools tool1 tool2 ...]"

summarise :: [TId] -> IO ()
summarise [] = Dir.getCurrentDirectory >>= Dir.getDirectoryContents >>= summarise
summarise xs = filterM Dir.doesDirectoryExist xs >>= gather >>= writeTable "table.html"

type PId = String
type TId = String

data Column = Column { cHead :: TId, cRows :: M.Map PId (Maybe Result)}
type DB     = [Column]

gather :: [FilePath] -> IO DB
gather ts = normalise <$> traverse gatherOne ts
  where
    gatherOne t = do
      fs <- lines <$> readCreateProcess (shell $ "find " <> t <> " -type f -name '*.result'") mempty
      rs <- (M.fromList . fmap (\r -> (rProblem r, Just r))) <$> traverse deserialise fs
      return $ Column{cHead = t, cRows = rs}
    normalise cs = fillMissing  `fmap` cs
      where
        allps = S.unions $ (M.keysSet . cRows) `fmap` cs
        dummy = M.fromList $ zip (S.toList allps) (repeat Nothing)
        fillMissing c = c{cRows = M.union (cRows c) dummy}

mkTable :: DB -> ([TId], [(PId, [Maybe Result])])
mkTable db              = M.assocs `fmap` foldr merge ([],M.empty) db
  where
    merge c (ts,ms) = (cHead c:ts, merge' (cRows c) ms)
    merge'          = M.mergeWithKey (\_ a b -> traceShow (a,b) $ Just (a:b)) (fmap (:[])) id

-- mkSummary :: DB -> Html
-- mkSummary = undefined

-- writeResults :: [Result] -> IO ()
-- writeResults = writeTable

writeTable :: FilePath -> DB -> IO()
writeTable fp db = do
  let
    fmap5 = fmap . fmap . fmap . fmap . fmap
  let (ts,ps) = fmap5 (show . rOutcome) $ mkTable db
  writeFile fp $ renderHtml
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
      $forall (p,rs) <- ps
        <tr>
          $forall mr <- rs
            <td>#{p}
              $maybe r <- mr
                <td>#{r}
              $nothing
                <td> missing
    |]

