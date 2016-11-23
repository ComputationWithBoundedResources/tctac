{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Summer where

import           Control.Arrow                 ((&&&))
import           Control.Monad                 (filterM, forM_, join)
import qualified Data.List                     as L (find)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (catMaybes)
import           Data.Monoid
import qualified Data.Set                      as S
import qualified System.Directory              as Dir
import           System.FilePath.Posix         ((<.>), (</>))
import           System.Process

import qualified Data.Text.Lazy.IO             as T (writeFile)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet
-- import           Text.Lucius

import           Paths_tctac
import           Runner
import           Util

-- import           Debug.Trace

data Summary = Summary
  { sFilePath :: FilePath
  , sTIds     :: [TId]
  , sCount    :: [Count] }

data Count = Count
  { cName :: String
  , cPred :: Outcome String -> Bool }

bigO :: [Count]
bigO = def <> const <> polys <> exp where
  def =
    [ Count{cName="MAYBE"   ,cPred=(== Maybe)}
    , Count{cName="TIMEOUT" ,cPred=(== Timeout)}
    , Count{cName="ERROR"   ,cPred = \o -> case o of {(Failure _) -> True; _ -> False}}]
  const = [ Count{cName="O(1)", cPred = (==Success "O(1)")} ]
  polys = [ Count{cName="O("++show i++")", cPred = isO i} | i <- [1..5] ]
  poly  = [ Count{cName="Poly", cPred = isPoly} ]
  exp   = [ Count{cName="Exp" , cPred = (==Success "EXP")} ]
  isO n o = o `elem` [Success ("O(" ++ show i ++ ")") | i <- [1..n]]
  isPoly (Success "Poly")      = True
  isPoly (Success ('O':'(':_)) = True
  isPoly _                     = False

summarise :: [TId] -> IO ()
summarise [] = Dir.getCurrentDirectory >>= Dir.getDirectoryContents >>= summarise
summarise xs = filterM Dir.doesDirectoryExist xs >>= gather >>= writeExperiments "table.html"

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
$doctype 5
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
  ^{table db ts ps}
  ^{summary db ts os}
  |]

writeExperiments :: FilePath -> DB -> IO ()
writeExperiments fp db = do
  forM_ ["experiments.css", "table.js", "sort_ascending.png", "sort_descending.png"] $
    \fn -> getDataFileName ("etc" </> fn) >>= flip copyFileIfMissing fn
  T.writeFile fp $ renderHtml $ renderExperiments db

