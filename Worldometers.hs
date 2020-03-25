{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module Worldometers (getData) where

import Network.HTTP.Simple

import System.IO 
import System.Exit
import System.Posix.Files
import System.Posix.Directory
import System.FilePath.Posix

import Control.Monad
import Control.Applicative

import Data.Maybe
import Data.List

import Data.Time.Clock
import Data.Time.Clock.POSIX

import qualified Data.Aeson as J

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import qualified Data.Attoparsec.Text as A

import Debug.Trace

import Series


getData :: [Text] -> IO (Map Item DataMap)
getData regions = foldM addRegion M.empty regions
  where addRegion map region = M.unionWith M.union map
          . fmap toDataMap <$> getSeries region
        toDataMap Series{..} = M.singleton serRegion Series{..}


getSeries :: Text -> IO (Map Item Series)
getSeries region = do
  let cachePath = cacheDir <> "/" <> T.unpack region <> ".csv"
  outdated cachePath >>= \case
    True -> do
      hPutStrLn stderr $ "Downloading " <> T.unpack region <> "..."
      createDirectoryRecursive cacheDir
      input <- getResponseBody <$> httpBS (url region)
      series <- case parseSeries region (T.decodeUtf8 input) of
        Left err -> exitWithError $ "Failed to parse input for " <> T.unpack region <> ": " <> err
        Right val -> pure val
      B.writeFile cachePath $ LB.toStrict $ J.encode series
      return series
    False -> J.decode <$> LB.readFile cachePath >>= \case
      Nothing -> exitWithError $ "Failed to parse cache file for " <> T.unpack region <> "!"
      Just series -> return series

url :: Text -> Request
url region = parseRequest_ $ "https://www.worldometers.info/coronavirus/country/" <> T.unpack region <> "/"


exitWithError :: String -> IO a
exitWithError msg = do
  hPutStrLn stderr msg
  exitWith $ ExitFailure 1


parseSeries :: Text -> Text -> Either String (Map Item Series)
parseSeries region = A.parseOnly (seriesMapP region)


cacheDir :: String
cacheDir = "data/worldometers"


createDirectoryRecursive :: String -> IO ()
createDirectoryRecursive dir = forM_ (tail $ inits $ splitPath dir) $ \ps -> do
  let path = joinPath ps
  exists <- fileExist path
  when (not exists) $ createDirectory path accessModes

outdated :: String -> IO Bool
outdated path = fileExist path >>= \case
  True -> do
    modified <- posixSecondsToUTCTime . modificationTimeHiRes
      <$> getFileStatus path
    now <- getCurrentTime
    return $ addUTCTime maxAge modified < now
  False -> return True

maxAge :: NominalDiffTime
maxAge = fromIntegral 1800


seriesMapP :: Text -> A.Parser (Map Item Series)
seriesMapP region = addRecovered . M.fromList
  . catMaybes <$> many (seriesP region)


seriesP :: Text -> A.Parser (Maybe (Item,Series))
seriesP region = do
  A.manyTill A.anyChar "categories: ["
  maybeSeriesP region <|> pure Nothing

maybeSeriesP :: Text -> A.Parser (Maybe (Item,Series))
maybeSeriesP serRegion = do
  serDates <- (A.char '"' *> A.takeWhile (/= '"') <* A.char '"')
    `A.sepBy` A.char ',' <* "]"
  A.manyTill A.anyChar "series: [{"
  A.skipSpace
  name <- getItem <$> ("name: '" *> A.takeWhile (/= '\'') <* "',")
  A.manyTill A.anyChar "data: ["
  serValues <- (A.decimal <|> ("nan" *> pure 0)) `A.sepBy` "," <* "]"

  return $ Just (fromJust name, Series{..})
  {-return $ case getItem name of
    Just item -> Just (item,Series{..})
    Nothing -> Nothing-}


getItem :: Text -> Maybe Item
getItem "Cases" = Just Confirmed
getItem "Deaths" = Just Deaths
getItem "Currently Infected" = Just Active
getItem name = Nothing


addRecovered :: Map Item Series -> Map Item Series
addRecovered map = case getRecovered map of
  Just series -> M.insert Recovered series map
  Nothing -> map

getRecovered :: Map Item Series -> Maybe Series
getRecovered m = do
  Series{serValues = confirmed, ..} <- M.lookup Confirmed m
  Series{serValues = deaths} <- M.lookup Deaths m
  Series{serValues = active} <- M.lookup Active m
  return Series { serValues = map (uncurry (-)) $ zip confirmed
                  $ map (uncurry (+)) $ zip active deaths, .. }
