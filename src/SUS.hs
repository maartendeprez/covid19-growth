{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase, TupleSections #-}

module SUS (getData) where

import Network.HTTP.Simple
import System.Posix.Directory
import System.IO

import Control.Applicative

import Data.Either
import Data.List

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


getData :: Text -> IO DataMap
getData "confirmed" = getSeries (\(_,_,x,_) -> x) <$> getXData
getData "deaths" = getSeries (\(_,_,_,x) -> x) <$> getXData
getData item = exitWithError $ "Item " <> T.unpack item
  <> " is not available from SUS"


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
  hPutStrLn stderr $ "Reading " <> file
  input <- B.readFile (cacheDir <> "/" <> file)

  case parseCSV (T.decodeUtf8 input) of
    Left err -> exitWithError $ "Failed to parse input: " <> err
    Right csv -> mapM_ (putStrLn . ("Invalid row: " <>) . T.unpack) invalid
      >> pure valid where (invalid,valid) = partitionEithers csv


cacheDir :: String
cacheDir  = "data/sus"

getUrl :: String -> Request
getUrl = parseRequest_ . ("https://covid.saude.gov.br/assets/files/" <>)

getFileName :: IO String
getFileName = last . sort . filter f <$> readDir cacheDir
  where f name = ( "COVID19_" `isPrefixOf` name
                   && ".csv" `isSuffixOf` name )
  

-- return $ "COVID19_20200415.csv" -- <> formatTime defaultTimeLocale "%Y%m%d"
--    (addUTCTime (-nominalDay) time) <> ".csv"
--    time <> ".csv"

readDir :: FilePath -> IO [FilePath]
readDir path = do
  dir <- openDirStream path
  files <- getFiles dir
  closeDirStream dir
  return files

getFiles :: DirStream -> IO [FilePath]
getFiles ds = readDirStream ds >>= \case
  "" -> pure []
  file -> (file:) <$> getFiles ds


type Row = (Text,Day,Int,Int)

parseCSV :: Text -> Either String [Either Text Row]
parseCSV = A.parseOnly (csvP <* A.endOfInput)

csvP :: A.Parser [Either Text Row]
csvP = headersP >> many ((Right <$> rowP) <|> (Left <$> invalidRowP))

headersP :: A.Parser ()
headersP = ("regiao;estado;data;casosNovos;casosAcumulados;obitosNovos;obitosAcumulados\r\n" >> pure ()) <|> fail "wrong headers"

rowP :: A.Parser Row
rowP = do
  A.takeWhile (/= ';') <* A.char ';'
  estado <- A.takeWhile (/= ';') <* A.char ';'
  date <- (dateP <|> date2P <|> dayP) <* A.char ';'
  A.decimal  <* A.char ';'
  casosAccumulados <- A.decimal  <* A.char ';'
  A.decimal  <* A.char ';'
  obitosAccumulados <- A.decimal  <* "\r\n"

  return ( estado, date
         , casosAccumulados,obitosAccumulados )

invalidRowP :: A.Parser Text
invalidRowP = T.pack <$> A.manyTill A.anyChar "\r\n"

dateP :: A.Parser Day
dateP = do
  day <- A.decimal <* A.char '/'
  month <- A.decimal <* A.char '/'
  year <- A.decimal
  return $ fromGregorian year month day

date2P :: A.Parser Day
date2P = do
  year <- A.decimal <* A.char '-'
  month <- A.decimal <* A.char '-'
  day <- A.decimal
  return $ fromGregorian year month day

dayP :: A.Parser Day
dayP = fromExcelDate <$> A.decimal

fromExcelDate :: Integer -> Day
fromExcelDate i = addDays i excelDateZero

excelDateZero :: Day
excelDateZero = fromGregorian 1900 1 1
