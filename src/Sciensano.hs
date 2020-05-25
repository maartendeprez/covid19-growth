{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase, TupleSections #-}

module Sciensano (getData) where

import Network.HTTP.Simple
import System.Posix.Directory
import System.Posix.Files
import System.IO

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LT

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import qualified Data.Attoparsec.Text as A

import Data.Aeson
import qualified Data.Aeson.Types as AT

import Data.Foldable
import Data.Either
import Data.Maybe

import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.LocalTime

import Series
import Common



getData :: Text -> IO DataMap
getData item = M.mapWithKey toSeries . foldr' save M.empty <$> getXData item
  where save r = M.insertWith (++) (fromString $ r `get` regionField item)
          [(date $ r `get` "DATE", round $ fromNumber $ r `get` item)]
        date = fromRight (error "invalid date") . A.parseOnly dateP . fromString
        dateP = fromGregorian <$> A.decimal <* "-" <*> A.decimal
          <* "-" <*> A.decimal <* A.endOfInput
        toSeries serRegion m = Series {..}
          where (serDates, serValues) = unzip m
        fromString (AT.String v) = v
        fromNumber (AT.Number n) = realToFrac n
        fromNumber (AT.String s) = read (T.unpack s)


getXData :: Text -> IO [Map Text Value]
getXData "CASES" = getXCases
getXData item = do
  let cachePath = cacheDir <> "/" <> file item
  outdated cachePath >>= \case
    True -> do
      hPutStrLn stderr $ "Downloading " <> file item <> "..."
      createDirectoryRecursive cacheDir
      input <- LT.encodeUtf8 . LT.decodeLatin1 . getResponseBody <$> httpLBS (url item)
      LB.writeFile cachePath input
      return $ decodeData input
    False -> decodeData <$> LB.readFile cachePath 


getXCases :: IO [Map Text Value]
getXCases = do
  tz <- getCurrentTimeZone
  now <- getCurrentTime
  let yesterday = addDays (-1) $ localDay $ utcToLocalTime tz now
  concat <$> mapM getDayCases [ fromGregorian 2020 03 31 .. yesterday ]

getDayCases :: Day -> IO [Map Text Value]
getDayCases day = do
  let (file,url) = casesUrl day
  let cache = cacheDir <> "/" <> file
  let date = AT.String $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" day
  input <- fileExist cache >>= \case
    True -> decodeData <$> LB.readFile cache
    False -> do
      hPutStrLn stderr $ "Downloading " <> file <> "..."
      createDirectoryRecursive cacheDir
      input <- LT.encodeUtf8 . LT.decodeLatin1 . getResponseBody <$> httpLBS url
      LB.writeFile cache input
      return $ decodeData input
  return $ map (M.insert "DATE" date) $ filter ("TX_DESCR_NL" `M.member`) $ input

decodeData :: LB.ByteString -> [Map Text Value]
decodeData = either error id . eitherDecode

url :: Text -> Request
url "TOTAL_IN" = "https://epistat.sciensano.be/Data/COVID19BE_HOSP.json"
url "TOTAL_IN_ICU" = "https://epistat.sciensano.be/Data/COVID19BE_HOSP.json"

file :: Text -> String
file "TOTAL_IN" = "COVID19BE_HOSP.json"
file "TOTAL_IN_ICU" = "COVID19BE_HOSP.json"

casesUrl :: Day -> (String,Request)
casesUrl day = ( file, setRequestPath ( B8.pack  $ "Data/" <> date <> "/" <> file )
                 "https://epistat.sciensano.be/" )
  where file = "COVID19BE_CASES_MUNI_CUM_" <> date <> ".json"
        date = formatTime defaultTimeLocale "%Y%m%d" day

regionField :: Text -> Text
regionField "TOTAL_IN" = "PROVINCE"
regionField "TOTAL_IN_ICU" = "PROVINCE"
regionField "CASES" = "TX_DESCR_NL"

cacheDir :: String
cacheDir = "data/sciensano"

get :: (Show k,Ord k) => Map k v -> k -> v
get m k = case M.lookup k m of
  Just v -> v
  Nothing -> error $ "missing " <> show k
