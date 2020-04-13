{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module Main where

import Options.Applicative

import System.Exit
import System.IO

import Text.Read
import Text.Printf

import Control.Monad
import Control.Applicative

import Debug.Trace

import Data.Maybe
import Data.List

import Data.Time.Calendar

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import qualified SUS
import qualified CSSE
import qualified Worldometers
import qualified Plotly
import qualified Vega

import Series
import Common


data Args = Args { argMode :: Mode
                 , argSource :: Source
                 , argSmoothing :: Int
                 , argAvgSmoothing :: Int
                 , argStart :: Int
                 , argDaily :: Bool
                 , argItem :: Item
                 , argRegions :: [Text]
                 } deriving Show

data Source = SCSSE | SWorldometers | SSus deriving Show
data Mode = MTable | MGraph deriving Show


main :: IO ()
main = do

  Args{..} <- execParser argsInfo

  dataMap <- case argSource of
    SCSSE -> CSSE.getData argItem
    SWorldometers -> M.lookup argItem <$> Worldometers.getData argRegions >>= \case
      Nothing -> exitWithError $ "Item " <> T.unpack (showItem argItem) <> " is not available from Worldometers"
      Just map -> pure map
    SSus -> SUS.getData argItem

  let (regions,invalidRegions) = partition (`M.member` dataMap) argRegions

  when (null regions) $ do
    hPutStrLn stderr "Error: no data found!"
    exitFailure
  
  when (not $ null invalidRegions) $
    hPutStrLn stderr ((case argSource of SCSSE -> "Warning: unknown region(s): "
                                         SWorldometers -> "Warning: missing data for region(s): ")
                      <> intercalate ", " (map T.unpack invalidRegions))

  let (dates,values) = align $ map (dataMap M.!) regions
      growths = map (getGrowths argStart argSmoothing argAvgSmoothing argDaily) values

  case argMode of
    MTable -> showGrowths dates regions growths
    MGraph -> Vega.showGraph argItem argDaily dates regions growths

argsInfo :: ParserInfo Args
argsInfo = info (helper <*> args)
      ( fullDesc <> progDesc "Calculate daily growth rates for the covid-19 pandemic"
        <> header "corona - calculate daily growth rates for the covid-19 pandemic" )

args :: Parser Args
args = Args
  <$> (flag' MGraph (long "graph" <> short 'g' <> help "Output a graph instead of a table.") <|> pure MTable)
  <*> option (maybeReader readSource) (long "source" <> short 'x' <> value SCSSE
                                       <> help "Where to obtain the input data (csse / worldometers).")
  <*> option auto (long "smoothing" <> short 's' <> value 3 <> showDefault
                   <> help "Over how many days to calculate growth rate.")
  <*> option auto (long "avg-smoothing" <> short 'a' <> value 3 <> showDefault
                   <> help "Over how many days to calculate daily increase / decrease averages.")
  <*> option auto (long "minimum" <> short 'm' <> value 50 <> showDefault
                   <> help "The minimum number to trigger the start of the series.")
  <*> switch (long "daily" <> short 'd' <> help "Calculate over daily increase / decreasy instead of total value.")
  <*> argument (maybeReader readItem) (metavar "ITEM" <> help "The input series (confirmed / active / recovered / deaths).")
  <*> some (strArgument (metavar "REGION" <> help "The region(s) to show"))


readSource :: String -> Maybe Source
readSource "csse" = Just SCSSE
readSource "worldometers" = Just SWorldometers
readSource "sus" = Just SSus
readSource _ = Nothing


align :: [Series] -> ([Text],[[Int]])
align series = (dates,values)
  where dates = map (T.pack . show) [minDate..maxDate]
        minDate = minimum $ map (head . serDates) series
        maxDate = maximum $ map (last . serDates) series
        values = map (\Series{..} -> map (const 0) [minDate .. addDays (-1) (head serDates)] ++ serValues
                                     ++ map (const 0) [addDays 1 (last serDates) .. maxDate]) series

showGrowths :: [Text] -> [Text] -> [[Maybe (Int, Maybe Double)]] -> IO ()
showGrowths dates regions growths = do
  putStrLn $ printf "%*s" dateWidth ("" :: String) <> intercalate " | "
    (map (printf "%*s" colWidth) regions)
  forM_ (zip dates $ transpose growths) $ \(date,growth) -> do
    when (not $ null $ catMaybes growth) $ do
      putStrLn $ printf "%8s: " date <> intercalate " | "
        (map showGrowth growth)


showGrowth :: Maybe (Int, Maybe Double) -> String
showGrowth Nothing = printf "%*s" colWidth ("" :: String)
showGrowth (Just (n,f)) = printf "%s (%6d)" (growth f) n
  where growth :: Maybe Double -> String
        growth Nothing = "        "
        growth (Just f)
          | isNaN f || isInfinite f = "        "
          | f >= 1 = printf "+%6.2f%%" $ 100 * (f - 1)
          | otherwise = printf "-%6.2f%%" $ 100 * (1 - f)

dateWidth = 10 :: Int
colWidth = 17 :: Int

getGrowths :: Int -> Int -> Int -> Bool -> [Int] -> [Maybe (Int, Maybe Double)]

getGrowths start len avg False = growths []
  where growths _ [] = []
        growths [] (n:ns)
          | n < start = Nothing : growths [] ns
          | otherwise = Just (n,Nothing) : growths [n] ns
        growths rs (n:ns)
          | length rs < len = Just (n,Nothing) : growths (rs ++ [n]) ns
          | otherwise = Just (n,Just f) : growths (tail rs ++ [n]) ns
            where f = (fromIntegral n / fromIntegral (head rs))
                    ** (1 / fromIntegral (length rs))

getGrowths start len avg True = growths []
  where growths _ [] = []
        growths [] (n:ns)
          | n < start = Nothing : growths [] ns
          | otherwise = Nothing : growths [n] ns
        growths rs (n:ns)
          | length rs < len + avg = Just (n - last rs, Nothing) : growths (rs ++ [n]) ns
          | otherwise = Just (n - last rs, Just f) : growths (tail rs ++ [n]) ns
            where f = ( fromIntegral (n - rs!!len) / fromIntegral (rs!!avg - head rs))
                    ** (1 / fromIntegral len)
