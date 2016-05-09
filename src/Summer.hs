{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Summer where

import           Control.Monad                 (filterM)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (catMaybes)
import           Data.Monoid
import qualified Data.Set                      as S
import qualified System.Directory              as Dir
import           System.FilePath.Posix         ((<.>), (</>))
import           System.Process
-- import           System.Environment (getArgs)

import qualified Data.Text.Lazy.IO             as T (writeFile)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet
-- import           Text.Lucius

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
      fs <- lines <$> readCreateProcess (shell $ "find " <> t <> " -type f -name '*.result'") mempty
      rs <- (M.fromList . fmap (\r -> (rProblem r, Just r))) <$> traverse deserialise fs
      return $ Column{cHead = t, cRows = rs}
    normalise cs = fillMissing  `fmap` cs
      where
        allps = S.unions $ (M.keysSet . cRows) `fmap` cs
        dummy = M.fromList $ zip (S.toList allps) (repeat Nothing)
        fillMissing c = c{cRows = M.union (cRows c) dummy}

mkTable :: DB -> ([TId], [(PId, [Maybe Result])])
mkTable db          = M.assocs `fmap` foldr merge ([],M.empty) db
  where
    merge c (ts,ms) = (cHead c:ts, merge' (cRows c) ms)
    merge'          = M.mergeWithKey (\_ a b -> traceShow (a,b) $ Just (a:b)) (fmap (:[])) id

-- mkSummary :: DB -> Html
-- mkSummary = undefined

-- writeResults :: [Result] -> IO ()
-- writeResults = writeTable

toEntry :: Result -> (String, Outcome String, String)
toEntry Result{rTool=t,rProblem=p,rTime=z,rOutcome=out} = (fp,out, showDouble z)
  where fp = tName t </> p <.> tExtension t

-- FIXME: MS: differentitate between error and proof when giving the link; I suppose shamlet can do Either
writeTable :: FilePath -> DB -> IO()
writeTable fp db = do
  let
    fmap5 = fmap . fmap . fmap . fmap . fmap
  let (ts,ps) = fmap5 toEntry $ mkTable db
  T.writeFile fp $ renderHtml
    [shamlet|
$doctype 5
<html>
  <link rel="stylesheet" type="text/css" href="http://cl-informatik.uibk.ac.at/software/tct/includes/experiments.css">
  <script type="text/javascript" src="http://cl-informatik.uibk.ac.at/software/tct/includes/table.js">
  <head>
    <title> experiment
  <body>
    <h1> Details
    <div class="experiments">
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
              <th class="table-sortable:numeric table-sortable">
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
                        <a href="#{l}.proof">
                          <div class="yes">#{out}
                      $of Maybe
                        <a href="#{l}.proof">
                          <div class="maybe">MAYBE
                      $of Failure err
                        <a href="#{l}.err">
                          <div class="error">ERROR
                      $of Timeout
                        <div class="timeout">TIMEOUT
                  <td>#{z}
                $nothing
                  <td>
                  <td> missing
                  <td> -
    |]

-- TODO: MS:
-- writeCss :: FilePath -> IO ()
-- writeCss fp =
--   T.writeFile fp $ renderCss $
--     [lucius|
-- /* general */
-- .experiments th.table-sortable {
--     background-image:url("http://cl-informatik.uibk.ac.at/software/tct/includes/sort_ascending.png");
--     background-repeat:no-repeat;
--     background-position:bottom left
-- }

-- .experiments th.table-sorted-asc {
--     background-image:url("http://cl-informatik.uibk.ac.at/software/tct/includes/sort_ascending.png");
--     background-repeat:no-repeat;
--     background-position:bottom left
-- }
-- .experiments th.table-sorted-desc {
--     background-image:url("http://cl-informatik.uibk.ac.at/software/tct/includes/sort_descending.png");
--     background-repeat:no-repeat;
--     background-position:bottom left
-- }

-- .experiments .toolname {
--  color: #2b5573;
--  vertical-align:middle;
-- }

-- .experiments .lhd {
--  text-align: right;
--  /* width:        200px; */
--  padding-right: 20px;
--  color: #2b5573;
-- }

-- .experiments .fileref {
--  color: #2b5573;
-- }

-- .experiments .rowsep {
--  height:        20px;
-- }

-- .experiments table {
--    border-collapse: separate;
-- }

-- .experiments th {
--  text-align: center;
--  color: #2b5573;
--  font-weight: normal;
--  vertical-align:bottom;
-- }

-- .summary th {
--  text-align: center;
--  color: #2b5573;
--  font-weight: normal;
--  vertical-align:bottom;
-- }

-- .experiments option {
--     padding-right: 0px;
--     font-size: 10px;
-- }


-- .experiments select {
--     padding-right: 0px;
--     font-size: 10px;
-- }


-- /* entries */

-- .experiments .missing {
--  color: #DDDDDD;
--  font-style: italic;
--  background-color: white;
-- }

-- .experiments .yes {
--  color: #009900;
--  font-style: italic;
--  background-color: white;
-- }

-- .experiments .no {
--  color: #990000;
--  font-style: italic;
--  background-color: white;
-- }

-- .experiments .maybe {
--  color: #FF9900;
--  font-style: italic;
--  background-color: white;
-- }

-- .experiments .timeout {
--  color: #FFBB00;
--  font-style: italic;
--  background-color: white;
-- }

-- .experiments .error {
--  color: #FF0000;
--  font-style: italic;
--  background-color: white;
-- }

-- .experiments .win {
--  color: #00FF00;
--  font-weight: bold;
--  border: 0.5mm solid #DDDDDD
-- }

-- .experiments .tooloutput {
--  font-family: monospace;
--  line-height: normal;
--  border: 2px dashed #333333;
--  padding-top: 10px;
--  padding-bottom: 10px;
--  padding-left: 10px;
--  padding-right: 10px;
--  background:   #DDDDDD;
-- }


-- /* exec times page */
-- .experiments .exectime {
--  color:   #666666;
-- }

-- .experiments .exectimes td {
--  text-align: center;
--  padding-left: 5px;
--  width:        60px;
--  padding-right: 5px;
-- }

-- .experiments .exectimes td {
--  color:   #666666;
--  text-align: center
-- }

-- /* comparison */
-- .comparison th {
--  text-align: center;
--  color: #2b5573;
--  font-weight: normal;
--  vertical-align:bottom;
-- }

-- .comparison .toolA {
--  color: #009900;
-- }

-- .comparison .toolB {
--  color: #990000;
-- }

-- .comparison .toolAB {
--  color: #FF9900;
-- }


-- .experiments .comparison td {
--  text-align: center;
--  padding-left: 5px;
--  width:        50px;
--  padding-right: 5px;
--  height:auto;
-- }


-- /* summary page */

-- .experiments .summary td {
--  text-align: center;
--  padding-left: 5px;
--  color:   #666666;
--  width:        60px;
--  padding-right: 5px;
-- }

-- /* results page */

-- .experiments .results a {
--  color:   #666666;
--  text-decoration: none;
-- }

-- .experiments .results .lhd {
--  text-align: right;
--  padding-right: 5px;
-- }


-- .experiments .results td {
--  text-align: center;
--  padding-left: 5px;
--  width:        60px;
--  padding-right: 5px;
--  height:auto;
-- }


-- /* result page */


-- .experiments .result td {
--  text-align: left;
--  padding-left: 5px;
-- }

-- .experiments .result h1 {
-- 	font-family: Arial, Helvetica, sans-serif;
-- 	color: #2b5573;
-- 	font-size: 14pt;
-- }
-- .experiments .result h2 {
-- 	font-family: Arial, Helvetica, sans-serif;
-- 	color: #2b5573;
-- 	font-size: 13pt;
-- }
-- .experiments .result h3 {
-- 	font-family: Arial, Helvetica, sans-serif;
-- 	color: #2b5573;
-- 	font-size: 12pt;
-- }
-- .experiments .result h4 {
-- 	font-family: Arial, Helvetica, sans-serif;
-- 	color: #2b5573;
-- 	font-size: 11pt;
-- }
-- .experiments .result h5 {
--         font-family: Arial, Helvetica, sans-serif;
-- 	color: #2b5573;
-- 	font-size: 10pt;
-- }

-- .horizontal {
--     behavior:url(-ms-transform.htc);
--     -moz-transform:rotate(-90deg) translateX(-20mm);
--     -webkit-transform:rotate(-90deg);
--     -o-transform:rotate(-90deg);
--     -ms-transform:rotate(-90deg);
--     text-align:right;
-- }
--     |]
--     undefined


--- *  summary -------------------------------------------------------------------------------------------------------

gatherPossibleAnswers :: DB -> [Outcome String]
gatherPossibleAnswers = S.toAscList . S.fromList . concatMap gatherOne
  where gatherOne Column{cRows=rows} = catMaybes . M.elems $ M.map (fmap rOutcome) rows


mkSummary :: DB -> ([TId],[(String,[Int])])
-- mkSummary :: DB -> ([TId],[(Outcome String,[Int])])
mkSummary db = let (a,b) = foldr mkColumn ([],[]) db in (a,zip (show `fmap` rs) b)
  where
    rs                 = gatherPossibleAnswers db
    mkColumn c (ts,zs) = (cHead c:ts, foldr (mkEntry $ fmap rOutcome $ catMaybes $ M.elems $ cRows c) [] rs : zs)
    mkEntry es r acc   = length (filter (r ==) es) : acc


writeSummary :: FilePath -> DB -> IO()
writeSummary fp db = do
  let (ts,ps) = mkSummary db
  T.writeFile fp $ renderHtml
    [shamlet|
$doctype 5
<html>
  <link rel="stylesheet" type="text/css" href="http://cl-informatik.uibk.ac.at/software/tct/includes/experiments.css">
  <script type="text/javascript" src="http://cl-informatik.uibk.ac.at/software/tct/includes/table.js">
  <head>
    <title> experiment
  <body>
    <h1> summary
    <table class="table-autofilter table-autosort table-filtered-rowcount:cnt">
      <thead>
        <tr>
          <th>
          $forall t <- ts
            <th colspan="1">
              <div class="toolname">#{t}
      <tbody>
        $forall (p,zs) <- ps
          <tr>
            <td>
              <div class="lhd">#{p}
            $forall z <- zs
                <td>
                  #{z}
    |]

