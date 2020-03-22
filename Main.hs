{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module Main where

import Options.Applicative
import System.IO

import Text.Read
import Text.Printf

import Control.Monad
import Control.Applicative

import Data.Maybe
import Data.List

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import Series
import qualified CSSE


data Args = Args { argMode :: Mode
                 , argSource :: Source
                 , argSmoothing :: Int
                 , argStart :: Int
                 , argItem :: Item
                 , argRegions :: [Text]
                 } deriving Show

data Source = SCSSE | SWorldometers deriving Show
data Mode = MTable | MGraph deriving Show


main :: IO ()
main = do

  Args{..} <- execParser argsInfo

  (dates,regions,values) <- case argSource of

    SCSSE -> do

      dataMap <- CSSE.getData argItem

      let (validRegions,invalidRegions) = partition (`M.member` dataMap) argRegions
      when (not $ null invalidRegions) $ hPutStrLn stderr $ "Warning: unknown region(s): "
        <> intercalate ", " (map T.unpack invalidRegions)

      let series = map (dataMap M.!) validRegions
      return ( serDates (head series), validRegions
             , map serValues series )

  let growths = map (getGrowths argStart argSmoothing) values

  case argMode of
    MTable -> showGrowths dates regions growths
    MGraph -> showGraph Args{..} dates regions growths

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
  <*> option auto (long "minimum" <> short 'm' <> value 50 <> showDefault
                   <> help "The minimum number to trigger the start of the series.")
  <*> argument (maybeReader readItem) (metavar "ITEM" <> help "The input series (confirmed / active / recovered / deaths).")
  <*> some (strArgument (metavar "REGION" <> help "The region(s) to show"))


readSource :: String -> Maybe Source
readSource "csse" = Just SCSSE
readSource "worldometers" = Just SWorldometers
readSource _ = Nothing


readItem :: String -> Maybe Item
readItem "confirmed" = Just Confirmed
readItem "active" = Just Active
readItem "recovered" = Just Recovered
readItem "deaths" = Just Deaths
readItem _ = Nothing


showItem :: Item -> Text
showItem Confirmed = "Confirmed Cases"
showItem Active = "Active Cases"
showItem Recovered = "Recoveries"
showItem Deaths = "Deaths"


showGraph :: Args -> [Text] -> [Text] -> [[Maybe (Int, Maybe Double)]] -> IO ()
showGraph Args{..} dates regions values = do

  let empties = minimum $ map (length . takeWhile emptyGrowth) values
      ymin = if argItem == Active then 0 else 1
      ymax = maybe 2 (+0.3) $ maximum $ map getGrowth $ concat values
      series = T.intercalate ", "  $ map (graphData (drop empties dates))
        $ zip regions $ map (drop empties) values
      title = "COVID-19 Growth of " <> showItem argItem

  T.putStrLn $ "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
  T.putStrLn $ "<html>"
  T.putStrLn $ "<head>"
  T.putStrLn $ "<meta charset=\"UTF-8\">"
  T.putStrLn $ "<title>" <> title <> "</title>"
  T.putStrLn $ "<script src=\"https://cdn.plot.ly/plotly-latest.min.js\"></script>"
  T.putStrLn $ "</head>"
  T.putStrLn $ "<body>"

  -- T.putStrLn $ "<h1>" <> title <> "</h1>"
  T.putStrLn $ "<div id=\"graph\"></div>"

  T.putStrLn $ "<script>"
  T.putStrLn $ "var data = [" <> series <> "];"
  T.putStrLn $ "var layout = { title:'" <> title <> "'"
    <> ", yaxis: { range: [" <> T.pack (show ymin) <> ", " <> T.pack (show ymax) <> "] }"
    <> ", shapes: [ { type: 'line', line: { color: 'red', width: 1 }"
    <> ", y0: 1, y1: 1, xref: 'paper', x0: 0, x1: 1 } ] };"
  T.putStrLn $ "Plotly.newPlot('graph', data, layout);"
  T.putStrLn $ "</script>"

  T.putStrLn $ "</html>"


emptyGrowth :: Maybe (Int, Maybe Double) -> Bool
emptyGrowth (Just (_, Just _)) = False
emptyGrowth _ = True


getGrowth :: Maybe (Int, Maybe Double) -> Maybe Double
getGrowth (Just (_, Just growth)) = Just growth
getGrowth _ = Nothing


graphData :: [Text] -> (Text,[Maybe (Int, Maybe Double)]) -> Text
graphData xs (title, ys) = "{ x: [" <> xdata <> "]"
  <> ", y: [" <> ydata <> "]"
  <> ", mode: 'lines+markers', name: '" <> title <> "' }"
  where xdata = T.intercalate ", " $ map (\x -> "'" <> x <> "'") xs
        ydata = T.intercalate ", " $ map showValue ys
        showValue (Just (_,Just growth)) = T.pack (show growth)
        showValue _ = "null"

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

getGrowths :: Int -> Int -> [Int] -> [Maybe (Int, Maybe Double)]
getGrowths start len = growths []
  where growths _ [] = []
        growths [] (n:ns)
          | n < start = Nothing : growths [] ns
          | otherwise = Just (n,Nothing) : growths [n] ns
        growths rs (n:ns)
          | length rs < len = Just (n,Nothing) : growths (rs ++ [n]) ns
          | otherwise = Just (n,Just f) : growths (tail rs ++ [n]) ns
            where f = (fromIntegral n / fromIntegral (head rs))
                    ** (1 / fromIntegral (length rs))
