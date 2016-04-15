{-# LANGUAGE QuasiQuotes #-}
module Summer where

import           Control.Monad                   (filterM)
import qualified Data.Map.Strict                 as M
import           Data.Monoid
import qualified Data.Set                        as S
import qualified System.Directory                as Dir
import           System.FilePath.Posix           ((<.>), (</>))
import           System.Process
-- import           System.Environment (getArgs)

import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Hamlet

import           Runner
import           Util

import           Debug.Trace


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
      fs <- lines <$> readCreateProcess (shell $ "find " <> t <> " -type f -name '*.result' -o -name '*.err'") mempty
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

toEntry :: Result -> (String, Outcome String, Double)
toEntry Result{rTool=t,rProblem=p,rTime=z,rOutcome=out} = (fp,out,z)
  where fp = tName t </> p <.> tExtension t

-- FIXME: MS: differentitate between error and proof when giving the link; I suppose shamlet can do Either
writeTable :: FilePath -> DB -> IO()
writeTable fp db = do
  let
    fmap5 = fmap . fmap . fmap . fmap . fmap
  let (ts,ps) = fmap5 toEntry $ mkTable db
  writeFile fp $ renderHtml
    [shamlet|
$doctype 5
<html>
  <link rel="stylesheet" type="text/css" href="http://cl-informatik.uibk.ac.at/software/tct/includes/experiments.css">
  <script type="text/javascript" src="http://cl-informatik.uibk.ac.at/software/tct/includes/table.js">
  <head>
    <title> experiment
  <body>
    <h1> Details
    <table class="table-autofilter table-autosort table-filtered-rowcount:cnt">
      <thead>
        <tr>
          <th>
          $forall t <- ts
            <th colspan="3">
              <div class="toolname">#{t}
        <tr>
          <th class="table-sortable:default table-sortable">
            <div class="lhd">Problem (<span id="cnt">all</span> selected)
          $forall t <- ts
            <th> *
            <th class="table-filterable">answer
            <th class="table-sortable:numeric table-sortable">time
      <tbody>
        $forall (p,rs) <- ps
          <tr>
            <td>
              <div class="lhd">#{p}
            $forall mr <- rs
              $maybe (l,o,z) <- mr
                <td>
                  <a href="#{l}">*
                <td>
                  $case o
                    $of Success out
                      <a href="#{l}.proof">#{out}
                    $of Failure err
                      <a href="#{l}.err">ERROR
                    $of Timeout
                      TIMEOUT
                <td>#{z}
              $nothing
                <td>
                <td> missing
                <td> -
    |]

