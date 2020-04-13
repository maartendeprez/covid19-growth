{-# LANGUAGE OverloadedStrings #-}

module Plotly (showGraph) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Series


showGraph :: Item -> Bool -> [Text] -> [Text] -> [[Maybe (Int, Maybe Double)]] -> IO ()
showGraph item daily dates regions values = do

  let empties = minimum $ map (length . takeWhile emptyGrowth) values
      ymin = if item == Active || daily then 0 else 1
      ymax = maybe 2 (+0.3) $ maximum $ map getGrowth $ concat values
      series = T.intercalate ", "  $ map (graphData (drop empties dates))
        $ zip regions $ map (drop empties) values
      title = "COVID-19 Growth of "
        <> (if daily then "daily " else "")
        <> showItem item

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
