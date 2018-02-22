{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Summer (summarise, diffBy) where

import           Control.Arrow                 ((&&&))
import           Control.Monad                 (filterM, forM_, join)
import qualified Data.List                     as L (find)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (catMaybes, isJust, fromMaybe)
import           Data.Monoid
import qualified Data.Set                      as S
import qualified System.Directory              as Dir
import           System.FilePath.Posix         ((<.>), (</>))
import           System.Process
import           System.Exit (die)
import           Text.Printf

import qualified Data.Text.Lazy.IO             as T (writeFile)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet

import           Paths_tctac
import           Runner
import           Util


summarise :: [TId] -> IO ()
summarise [] = Dir.getCurrentDirectory >>= Dir.getDirectoryContents >>= summarise
summarise xs = filterM Dir.doesDirectoryExist xs >>= gather >>= writeExperiments "table.html"

-- compares the 'Outcome's of two tools -- for "Golden Tests"
-- for each problem
--   * ignores it if the Outcome is equal
--   * prints the results in green if the first tool is "better"
--   * prints the results in red if the first tool is "worse"
diffBy :: (Outcome String -> Outcome String -> Ordering) -> TId -> TId -> IO ()
diffBy cmp t1 t2 = do
  unlessM (Dir.doesDirectoryExist t1) $ die (t1 ++ " show does not exist.")
  unlessM (Dir.doesDirectoryExist t2) $ die (t2 ++ " show does not exist.")
  db <- gather [t1,t2]
  let 
    ps  = queryProblems db
    len = succ $ maximum $ length`fmap` ps

  forM_ ps $ \p -> do
    let 
      r1 = queryOutcome db t1 p
      r2 = queryOutcome db t2 p
    case cmp r1 r2 of
      EQ -> pure ()
      LT -> putStrLn $ row len green p r1 r2
      GT -> putStrLn $ row len red   p r1 r2
  where
  queryOutcome db t p = Failure "DoesNotExist" `fromMaybe` ( (\(_,r,_) -> r) <$> queryEntry db t p)
  fill n x            = x ++ take (n - length x) (repeat ' ')
  row len code p r1 r2 = fill len p ++ code (fill len (show r1) ++ fill len (show r2))
  red   xs = "\ESC[31m" ++ xs ++ "\ESC[m"
  green xs = "\ESC[32m" ++ xs ++ "\ESC[m"

type PId = String
type TId = String

data Column = Column { cHead :: TId, cRows :: M.Map PId (Maybe Result)}
type DB     = [Column]

gather :: [FilePath] -> IO DB
gather = traverse gatherOne where
  gatherOne t = do
    fs <- lines <$> readCreateProcess (shell $ "find " <> t <> " -type f -name '*.result'") mempty
    rs <- (M.fromList . fmap (rProblem &&& Just)) <$> traverse deserialise fs
    return Column{cHead = t, cRows = rs}

queryTools :: DB -> [TId]
queryTools = fmap cHead

queryProblems :: DB -> [PId]
queryProblems = S.toList . S.unions . fmap (M.keysSet . cRows)

queryOutcomes :: DB -> [Outcome String]
queryOutcomes = S.toList . S.unions . fmap k
  where k = S.fromList . fmap rOutcome . catMaybes . M.elems . cRows

toEntry :: Result -> (String, Outcome String, String)
toEntry Result{rTool=t,rProblem=p,rTime=z,rOutcome=out} = (fp,out, showDouble z)
  where fp = tName t </> p <.> tExtension t

queryResult :: DB -> TId -> PId -> Maybe Result
queryResult db t p = do
  es <- cRows <$> L.find ((==t) . cHead) db
  join (M.lookup p es)

queryEntry :: DB -> TId -> PId ->  Maybe (String, Outcome String, String)
queryEntry db t p = fmap toEntry (queryResult db t p)

queryNum :: DB -> TId -> Outcome String -> Int
queryNum db t o = length $ filter ((==Just o) . fmap rOutcome) es
  where es = maybe [] (M.elems . cRows) $ L.find ((==t) . cHead) db

queryTime :: DB -> TId -> Maybe (Outcome String) -> String
queryTime db t mOutcome = printf "%.2f\n" $ avg $ map getTime $ filter filt es
  where es = maybe [] (M.elems . cRows) $ L.find ((==t) . cHead) db
        avg xs | null xs = 0
               | otherwise = sum xs / fromIntegral (length xs)
        getTime (Just x) = rTime x
        getTime _        = 0
        filt | isJust mOutcome = (==mOutcome) . fmap rOutcome
             | otherwise = const True

header :: Html
header =
  [shamlet|
$doctype 5
<html>
  <link rel="stylesheet" type="text/css" href="experiments.css">
  <script type="text/javascript" src="table.js">
  <head>
    <title> experiment
  <body>
  |]

table :: DB -> [TId] -> [PId] -> Html
table db ts ps =
    [shamlet|
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
      $forall p <- ps
        <tr>
          <td>
            <div class="lhd">#{p}
          $forall t <- ts
            $maybe (l,o,z) <- queryEntry db t p
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

summary :: DB -> [TId] -> [Outcome String] -> Html
summary db ts os =
  [shamlet|
<br /><h3>Result Summary</h3><br />
<div class="summary">
  <table>
    <theadd>
      <tr>
        <th>
        $forall t <- ts
          <th><div class="toolname">#{t}
    <tbody>
      $forall o <- os
        <tr>
          <td><div class="lhd">#{show o}
          $forall t <- ts
            <td>#{queryNum db t o}
<br />
<h3>Average Times</h3><br />
<div class="summary">
  <table>
    <theadd>
      <tr>
        <th>
        $forall t <- ts
          <th><div class="toolname">#{t}
    <tbody>
      $forall o <- os
        <tr>
          <td><div class="lhd">#{show o}
          $forall t <- ts
            <td>#{queryTime db t (Just o)}
      <tr>
        <td><div class="lhd">Overall
        $forall t <- ts
          <td>#{queryTime db t Nothing}

  |]

renderExperiments :: DB -> Html
renderExperiments db =
  let
    ts = queryTools db
    ps = queryProblems db
    os = queryOutcomes db
  in
  [shamlet|
^{header}
  ^{summary db ts os}
  ^{table db ts ps}
  |]

writeExperiments :: FilePath -> DB -> IO ()
writeExperiments fp db = do
  forM_ ["experiments.css", "table.js", "sort_ascending.png", "sort_descending.png"] $
    \fn -> getDataFileName ("etc" </> fn) >>= flip copyFileIfMissing fn
  T.writeFile fp $ renderHtml $ renderExperiments db

