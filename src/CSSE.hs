{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module CSSE (getData) where

import Network.HTTP.Simple

import System.IO 
import System.Posix.Files
import System.Posix.Directory
import System.FilePath.Posix
import System.Exit

import Control.Monad
import Control.Applicative

import Data.List

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar

import qualified Data.Attoparsec.Text as A

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Series
import Common


getData :: Text -> IO DataMap
getData "confirmed" = getXData "confirmed"
getData "recovered" = getXData "recovered"
getData "deaths" = getXData "deaths"
getData "active" = getActive
  <$> getXData "confirmed"
  <*> getXData "recovered"
  <*> getXData "deaths"

getActive :: DataMap -> DataMap -> DataMap -> DataMap
getActive confirmed recovered deaths = M.intersectionWith active (M.intersectionWith active confirmed recovered) deaths
  where active Series{serValues = confirmed, ..} Series{serValues = recovered}
          = Series {serValues = map (uncurry (-)) $ zip confirmed recovered, ..}

getXData :: Text -> IO DataMap
getXData item = do

  let cachePath = cacheDir <> "/" <> cache item
  input <- outdated cachePath >>= \case
    True -> do
      hPutStrLn stderr $ "Downloading " <> cache item <> "..."
      createDirectoryRecursive cacheDir
      download <- getResponseBody <$> httpBS (url item)
      B.writeFile cachePath download
      return download
    False -> B.readFile (cacheDir <> "/" <> cache item)

  (hdrs:rows) <- case parseCSV input of
    Right res -> return res
    Left err -> do
      hPutStrLn stderr $ "Failed to parse input: " <> err
      exitWith $ ExitFailure 1
    
  return $ M.fromList $ map (getSeries $ map parseDate $ drop 4 hdrs) rows

parseCSV :: ByteString -> Either String [[Text]]
parseCSV = A.parseOnly (csv <* A.endOfInput) . T.decodeUtf8
  where csv = some (row <* A.char '\n')
        row = col `A.sepBy` ","
        col = (A.char '"' *> A.takeWhile (/= '"') <* A.char '"')
          <|> A.takeWhile (`notElem` (",\r\n" :: String))

parseDate :: Text -> Day
parseDate t = case T.splitOn "/" t of
  [month,day,year] -> fromGregorian
    (read $ "20" <> T.unpack year)
    (read $ T.unpack month)
    (read $ T.unpack day)


getSeries :: [Day] -> [Text] -> (Text,Series)
getSeries serDates (state:country:_:_:values) = (serRegion, Series {..})
  where serValues = map (read . T.unpack) values
        serRegion
          | T.null state || state == country = fixedCountry
          | otherwise = state <> "," <> fixedCountry
        fixedCountry
          | ", " `T.isInfixOf` country = T.intercalate " " $ reverse $ T.splitOn ", " country
          | otherwise = country

url :: Text -> Request
url "confirmed" = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
url "recovered" = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
url "deaths" = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

cache :: Text -> String
cache "confirmed" = "confirmed.csv"
cache "recovered" = "recovered.csv"
cache "deaths" = "deaths.csv"

cacheDir :: String
cacheDir = "data/csse"
