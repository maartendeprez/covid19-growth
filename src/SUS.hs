{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase, TupleSections #-}

module SUS (getData) where

import Network.HTTP.Simple
import System.IO

import Control.Applicative

import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar

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


getData :: Item -> IO DataMap
getData Confirmed = getSeries (\(_,_,x,_) -> x) <$> getXData
getData Deaths = getSeries (\(_,_,_,x) -> x) <$> getXData
getData item = exitWithError $ "Item " <> T.unpack (showItem item) <> " is not available from SUS"


getSeries :: (Row -> Int) -> [Row] -> DataMap
getSeries f = foldr insert M.empty
  where insert row@(region,day,_,_) map = case M.lookup region map of
          Just Series{..} -> M.insert region Series
            { serDates = day : serDates
            , serValues = f row : serValues
            , .. } map
          Nothing -> M.insert region Series
            { serDates = [day]
            , serValues = [f row]
            , serRegion = region } map


getXData :: IO [Row]
getXData = do
  file <- getFileName
  let cachePath = cacheDir <> "/" <> file
  input <- outdated cachePath >>= \case
    True -> do
      hPutStrLn stderr $ "Downloading SUS data..."
      createDirectoryRecursive cacheDir
      download <- getResponseBody <$> httpBS (getUrl file)
      B.writeFile cachePath download
      return download
    False -> B.readFile (cacheDir <> "/" <> file)

  case parseCSV (T.decodeUtf8 input) of
    Left err -> exitWithError $ "Failed to parse input: " <> err
    Right csv -> pure csv


cacheDir :: String
cacheDir  = "data/sus"

getUrl :: String -> Request
getUrl = parseRequest_ . ("https://covid.saude.gov.br/assets/files/" <>)

getFileName :: IO String
getFileName = do
  time <- getCurrentTime
  return $ "COVID19_" <> formatTime defaultTimeLocale "%Y%m%d" time <> ".csv"

type Row = (Text,Day,Int,Int)

parseCSV :: Text -> Either String [Row]
parseCSV = A.parseOnly (csvP <* A.endOfInput)

csvP :: A.Parser [Row]
csvP = headersP >> many rowP

headersP :: A.Parser ()
headersP = "regiao;estado;data;casosNovos;casosAcumulados;obitosNovos;obitosAcumulado\r\n" >> pure ()

rowP :: A.Parser Row
rowP = do
  A.takeWhile (/= ';') <* A.char ';'
  estado <- A.takeWhile (/= ';') <* A.char ';'
  day <- A.decimal <* A.char '/'
  month <- A.decimal <* A.char '/'
  year <- A.decimal <* A.char ';'
  A.decimal  <* A.char ';'
  casosAccumulados <- A.decimal  <* A.char ';'
  A.decimal  <* A.char ';'
  obitosAccumulados <- A.decimal  <* "\r\n"

  return ( estado, fromGregorian year month day
         , casosAccumulados,obitosAccumulados )
