{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Summer where

import           Control.Monad                 (filterM, forM_)
import qualified Data.Map.Strict               as M
-- import           Data.Maybe                    (catMaybes)
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

import           Paths_tctac
import           Runner
import           Util

-- import           Debug.Trace


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
    merge'          = M.mergeWithKey (\_ a b -> Just (a:b)) (fmap (:[])) id

-- mkSummary :: DB -> Html
-- mkSummary = undefined

-- writeResults :: [Result] -> IO ()
-- writeResults = writeTable

toEntry :: Result -> (String, Outcome String, String)
toEntry Result{rTool=t,rProblem=p,rTime=z,rOutcome=out} = (fp,out, showDouble z)
  where fp = tName t </> p <.> tExtension t

writeTable :: FilePath -> DB -> IO()
writeTable fp db = do
  forM_ ["experiments.css", "table.js", "sort_ascending.png", "sort_descending.png"] $
    \fn -> getDataFileName ("etc" </> fn) >>= flip copyFileIfMissing fn
  let
    fmap5 = fmap . fmap . fmap . fmap . fmap
    (ts,ps) = fmap5 toEntry $ mkTable db
  T.writeFile fp $ renderHtml
    [shamlet|
$doctype 5
<html>
  <link rel="stylesheet" type="text/css" href="experiments.css">
  <script type="text/javascript" src="table.js">
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


--- *  summary -------------------------------------------------------------------------------------------------------
-- TODO

-- gatherPossibleAnswers :: DB -> [Outcome String]
-- gatherPossibleAnswers = S.toAscList . S.fromList . concatMap gatherOne
--   where gatherOne Column{cRows=rows} = catMaybes . M.elems $ M.map (fmap rOutcome) rows

-- mkSummary :: DB -> ([TId],[(String,[Int])])
-- -- mkSummary :: DB -> ([TId],[(Outcome String,[Int])])
-- mkSummary db = let (a,b) = foldr mkColumn ([],[]) db in (a,zip (show `fmap` rs) b)
--   where
--     rs                 = gatherPossibleAnswers db
--     mkColumn c (ts,zs) = (cHead c:ts, foldr (mkEntry $ fmap rOutcome $ catMaybes $ M.elems $ cRows c) [] rs : zs)
--     mkEntry es r acc   = length (filter (r ==) es) : acc

--   -- <link rel="stylesheet" type="text/css" href="http://cl-informatik.uibk.ac.at/software/tct/includes/experiments.css">
--   -- <script type="text/javascript" src="http://cl-informatik.uibk.ac.at/software/tct/includes/table.js">

-- writeSummary :: FilePath -> DB -> IO()
-- writeSummary fp db = do
--   let (ts,ps) = mkSummary db
--   T.writeFile fp $ renderHtml
--     [shamlet|
-- $doctype 5
-- <html>
--   <link rel="stylesheet" type="text/css" href="experiments.css">
--   <script type="text/javascript" src="table.js">
--   <head>
--     <title> experiment
--   <body>
--     <h1> summary
--     <table class="table-autofilter table-autosort table-filtered-rowcount:cnt">
--       <thead>
--         <tr>
--           <th>
--           $forall t <- ts
--             <th colspan="1">
--               <div class="toolname">#{t}
--       <tbody>
--         $forall (p,zs) <- ps
--           <tr>
--             <td>
--               <div class="lhd">#{p}
--             $forall z <- zs
--                 <td>
--                   #{z}
--     |]

