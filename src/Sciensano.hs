{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase, TupleSections #-}

module Sciensano (getData) where

import Network.HTTP.Simple
import System.Posix.Directory
import System.IO

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LT

import qualified Data.ByteString.Lazy as LB

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import qualified Data.Attoparsec.Text as A

import Data.Aeson
import qualified Data.Aeson.Types as AT

import Data.Foldable
import Data.Either
import Data.Maybe
import Data.Time.Calendar

import Series
import Common



getData :: Text -> IO DataMap
getData item = M.mapWithKey toSeries . foldr' save M.empty <$> getXData
  where save r = M.insertWith (++) (fromString $ r M.! "PROVINCE")
          [(date $ r M.! "DATE", round $ fromNumber $ r M.! item)]
        date = fromRight (error "invalid date") . A.parseOnly dateP . fromString
        dateP = fromGregorian <$> A.decimal <* "-" <*> A.decimal
          <* "-" <*> A.decimal <* A.endOfInput
        toSeries serRegion m = Series {..}
          where (serDates, serValues) = unzip m
        fromString (AT.String v) = v
        fromNumber (AT.Number n) = realToFrac n


getXData :: IO [Map Text Value]
getXData = do
  let cachePath = cacheDir <> "/" <> file
  outdated cachePath >>= \case
    True -> do
      hPutStrLn stderr $ "Downloading " <> file <> "..."
      createDirectoryRecursive cacheDir
      input <- LT.encodeUtf8 . LT.decodeLatin1 . getResponseBody <$> httpLBS url
      LB.writeFile cachePath input
      return $ decodeData input
    False -> decodeData<$> LB.readFile cachePath 
  where decodeData = either error id . eitherDecode

url :: Request
url = "https://epistat.sciensano.be/Data/COVID19BE_HOSP.json"

file :: String
file = "COVID19BE_HOSP.json"

cacheDir :: String
cacheDir = "data/sciensano"
