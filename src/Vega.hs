{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Vega (showGraph) where

import Data.Maybe

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import NeatInterpolation

import Series


showGraph :: Item -> Bool -> [Text] -> [Text] -> [[Maybe (Int, Maybe Double)]] -> IO ()
showGraph item daily dates regions values = do

  let empties = minimum $ map (length . takeWhile emptyGrowth) values
      ymin = T.pack $ show $ if item == Active || daily then 0 else 1
      ymax = T.pack $ show $ maybe 2 (+0.3) $ maximum $ map getGrowth $ concat values
      xmin = head $ drop empties dates
      xmax = last dates
      series = T.intercalate ", "
        $ concatMap (graphData dates)
        $ zip regions values
      title = "COVID-19 Growth of "
        <> (if daily then "daily " else "")
        <> showItem item

  T.putStrLn $ [text|
    <?xml version="1.0" encoding="UTF-8" ?>
    <html>
      <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <title>$title</title>
        <script src="https://cdn.jsdelivr.net/npm/vega@5"></script>
        <script src="https://cdn.jsdelivr.net/npm/vega-lite@4"></script>
        <script src="https://cdn.jsdelivr.net/npm/vega-embed@6"></script>
      </head>
      <body>
        <div id="graph" style="overflow: hidden; padding: 0; width: 100%; height: 100%;"></div>
        <script>
          vegaEmbed('#graph', {
            $$schema: 'https://vega.github.io/schema/vega-lite/v4.json',
            title: "$title",
            width: 'container',
            height: 'container',
            data: {
              values: [ $series ]
            },
            layer: [
              {
                encoding: {
                  x: { field: 'date', type: 'temporal', title: 'Date',
                       scale: { domain: ["$xmin", "$xmax"] } },
                  y: { field: 'growth', type: 'quantitative', title: 'Growth',
                       scale: { domain: [$ymin, $ymax] } },
                  color: { field: 'region', type: 'nominal', title: 'Region' },
                },
                layer: [
                  {
                    mark: 'line',
                    encoding: {
                      opacity: {
                        condition: { selection: 'highlight', value: 1 },
                        value: 0.1
                      }
                    },
                    selection: {
                      grid: {
                        type: 'interval',
                        bind: 'scales',
                      },
                      highlight: {
                        type: 'multi',
                        empty: 'all',
                        fields: [ 'region' ],
                        bind: 'legend',
                      }
                    }
                  },
                  {
                    mark: 'point',
                    encoding: {
                      opacity: {
                        condition: {
                          test: {
                            and: [
                              { selection: 'highlight' },
                              { selection: 'hover' }
                            ]
                          },
                          value: 1
                        },
                        value: 0
                      }
                    },
                    selection: {
                      hover: {
                        type: 'single',
                        encodings: [ 'x' ],
                        nearest: true,
                        on: 'mouseover',
                        empty: 'none',
                        clear: 'mouseout'
                      }
                    }
                  }
                ]
              },
              {
                transform: [
                  {
                    filter: {
                      and: [
                        { selection: 'hover' },
                        { selection: 'highlight' }
                      ]
                    }
                  }
                ],
                layer: [
                  {
                    encoding: {
                      x: { field: 'date', type: 'temporal', aggregate: 'min' },
                    },
                    mark: { type: 'rule', color: 'gray' }
                  },
                  {
                    encoding: {
                      x: { field: 'date', type: 'temporal' },
                      y: { field: 'growth', type: 'quantitative' },
                      color: { field: 'region', type: 'nominal' },
                      text: { type: 'quantitative', field: 'growth', format: '.3f' },
                    },
                    mark: {
                      type: 'text',
                      strokeWidth: 2,
                      align: 'right',
                      dx: -5, dy: -5
                    }
                  },
                ]
              }
            ]
          },
          {
            rendered: 'svg'
          });
        </script>
      </body>
    </html>
  |]


emptyGrowth :: Maybe (Int, Maybe Double) -> Bool
emptyGrowth (Just (_, Just _)) = False
emptyGrowth _ = True


getGrowth :: Maybe (Int, Maybe Double) -> Maybe Double
getGrowth (Just (_, Just growth)) = Just growth
getGrowth _ = Nothing


graphData :: [Text] -> (Text,[Maybe (Int, Maybe Double)]) -> [Text]
graphData xs (name,ys) = mapMaybe point $ zip xs ys 
  where point (x,(Just (_,Just y))) = Just
          $ "{ region: \"" <> name <> "\", "
          <> " date: \"" <> x <> "\", "
          <> "growth: " <> T.pack (show y) <> " }"
        point _ = Nothing
