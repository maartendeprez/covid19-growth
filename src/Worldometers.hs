{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase, TupleSections #-}

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
import Data.Time.Calendar

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

import Series
import Common

getData :: [Text] -> IO (Map Text DataMap)
getData regions = foldM addRegion M.empty regions
  where addRegion map region = M.unionWith M.union map
          . fmap toDataMap <$> getSeries region
        toDataMap Series{..} = M.singleton serRegion Series{..}


getSeries :: Text -> IO (Map Text Series)
getSeries region = do
  let cachePath = cacheDir <> "/" <> T.unpack region <> ".csv"
  outdated cachePath >>= \case
    True -> do
      hPutStrLn stderr $ "Downloading " <> T.unpack region <> "..."
      createDirectoryRecursive cacheDir
      input <- getResponseBody <$> httpBS (url region)
      series <- case parseSeries region (T.decodeUtf8 input) of
        Left err -> exitWithError $ "Failed to parse input for "
          <> T.unpack region <> ": " <> err
        Right graphs -> pure $ addRecovered $ M.fromList
          $ mapMaybe (\(name,vals) -> (,vals) <$> getItem name)
          $ M.toList graphs
      B.writeFile cachePath $ LB.toStrict $ J.encode series
      return series
    False -> J.decode <$> LB.readFile cachePath >>= \case
      Nothing -> exitWithError $ "Failed to parse cache file for " <> T.unpack region <> "!"
      Just series -> return series

url :: Text -> Request
url region = parseRequest_ $ "https://www.worldometers.info/coronavirus/country/" <> T.unpack region <> "/"


parseSeries :: Text -> Text -> Either String (Map Text Series)
parseSeries region = A.parseOnly (seriesMapP region)


cacheDir :: String
cacheDir = "data/worldometers"


seriesMapP :: Text -> A.Parser (Map Text Series)
seriesMapP region = M.fromList <$> many (seriesP region)


seriesP :: Text -> A.Parser (Text,Series)
seriesP region = do
  A.manyTill A.anyChar "categories: ["
  maybeSeriesP region <|> seriesP region

maybeSeriesP :: Text -> A.Parser (Text,Series)
maybeSeriesP serRegion = do

  serDates <- (A.char '"' *> dateP <* A.char '"')
    `A.sepBy` A.char ',' <* "]"
  A.manyTill A.anyChar "series: [{"
  A.skipSpace
  name <- ("name: '" *> A.takeWhile (/= '\'') <* "',")
  A.manyTill A.anyChar "data: ["
  serValues <- (A.decimal <|> ("nan" *> pure 0)) `A.sepBy` "," <* "]"

  return (name, Series{..})


dateP :: A.Parser Day
dateP = fromGregorian 2020 <$> monthP <*> (A.char ' ' >> A.decimal)


monthP :: A.Parser Int
monthP = ("Jan" *> pure 1)
  <|> ("Feb" *> pure 2)
  <|> ("Mar" *> pure 3)
  <|> ("Apr" *> pure 4)
  <|> ("May" *> pure 5)
  <|> ("Jun" *> pure 6)
  <|> ("Jul" *> pure 7)
  <|> ("Aug" *> pure 8)
  <|> ("Sep" *> pure 9)
  <|> ("Oct" *> pure 10)
  <|> ("Nov" *> pure 11)
  <|> ("Dec" *> pure 12)


getItem :: Text -> Maybe Text
getItem "Cases" = Just "confirmed"
getItem "Deaths" = Just "deaths"
getItem "Currently Infected" = Just "active"
getItem name = Nothing


addRecovered :: Map Text Series -> Map Text Series
addRecovered map = case getRecovered map of
  Just series -> M.insert "recovered" series map
  Nothing -> map

getRecovered :: Map Text Series -> Maybe Series
getRecovered m = do
  Series{serValues = confirmed, ..} <- M.lookup "confirmed" m
  Series{serValues = deaths} <- M.lookup "deaths" m
  Series{serValues = active} <- M.lookup "active" m
  return Series { serValues = map (uncurry (-)) $ zip confirmed
                  $ map (uncurry (+)) $ zip active deaths, .. }
