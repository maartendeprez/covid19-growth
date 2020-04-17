{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Vega (showGraph) where

import Prelude hiding (filter)

import Data.Maybe

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Data.Aeson
import qualified Data.Aeson.Text as A

import Graphics.Vega.VegaLite

import Series


showGraph :: Item -> Bool -> [Text] -> [Text] -> [[Maybe (Double, Maybe Double)]] -> IO ()
showGraph item daily dates regions values = TL.putStrLn $ toFullScreenHtmlWith titleStr (Just opts) graph
  where graph = toVegaLite [ title titleStr []
                           , widthOfContainer, heightOfContainer
                           , layer [ asSpec [ dataFromRows [] dataRows
                                            , layer [ dataLayer, ruleLayer ] ]
                                   , refLayer ]
                           ]


        opts = object [ {-"renderer" .= ("svg" :: Text)-} ]

        dataRows = concatMap (graphData dates) $ zip regions values
        dataLayer = asSpec [ dataEnc [], layer [ lineLayer , pointLayer ] ]
        dataEnc = encoding
          . position X [ PName "Date", PmType Temporal, PTimeUnit (Utc YearMonthDate)
                       , PTitle "Date", PScale [ SDomain $ DStrings [xmin, xmax] ] ]
          . position Y [ PName "Growth", PmType Quantitative
                       , PScale [ SDomain $ DNumbers [ymin, ymax] ] ]
          . color [ MName "Region", MmType Nominal ]

        lineLayer = asSpec [ mark Line [], lineEnc [], lineSel [] ]
        lineEnc = encoding
          . opacity [ MSelectionCondition (SelectionName "Highlight")
                      [ MNumber 1.0 ]
                      [ MNumber 0.1 ] ]
        lineSel = selection
          . select "Grid" Interval [BindScales]
          . select "Highlight" Multi [ BindLegend (BLField "Region") ]

        pointLayer = asSpec [ mark Point [], pointEnc [] ]
        pointEnc = encoding . opacity [ MDataCondition
                                        [ (And ( Selection "Highlight" ) ( Selection "Hover" ), [ MNumber 1.0 ])
                                        , (Selection "Hover", [ MNumber 0.2 ])]
                                        [ MNumber 0.0 ] ]
        hover = ( And ( Selection "Highlight" ) ( Selection "Hover" ) )

        ruleLayer = asSpec [ transform
                             -- $ filter ( FCompose (Selection "Highlight") )
                             $ pivot "Region" "Growth" [ PiGroupBy ["Date"] ]
                             $ []
                           , encoding $ position X [ PName "Date", PmType Temporal ]
                             $ opacity [ MDataCondition [(Selection "Hover", [MNumber 1])]
                                         [MNumber 0] ]
                             $ tooltips ( [ TName "Date", TmType Temporal ]
                                          : [ [TName region, TmType Quantitative, TFormat ".3f" ]
                                            | region <- regions ] ) $ []
                           , mark Rule [ MColor "gray", MTooltip TTData ]
                           , selection $ select "Hover" Single [ Fields ["Date"], Nearest True, Empty
                                                               , On "mouseover", Clear "mouseout" ]
                             $ []
                           ]

        refLayer = asSpec [ dataFromColumns [] $ dataColumn "Growth" (Numbers refs) $ []
                          , mark Rule [ MColor "red", MSize 1 ]
                          , encoding $ position Y [ PName "Growth", PmType Quantitative ] $ []
                          ]

        refs = [if item == Active then 0 else 1]
        empties = minimum $ map (length . takeWhile emptyGrowth) values

        ymin = if item == Active || daily then 0 else 1
        ymax = maybe 2 (+0.3) $ maximum $ map getGrowth $ concat values
        xmin = head $ drop empties dates
        xmax = last dates

        titleStr = "COVID-19 Growth of "
          <> (if daily then "daily " else "")
          <> showItem item


emptyGrowth :: Maybe (Double, Maybe Double) -> Bool
emptyGrowth (Just (_, Just _)) = False
emptyGrowth _ = True


getGrowth :: Maybe (Double, Maybe Double) -> Maybe Double
getGrowth (Just (_, Just growth)) = Just growth
getGrowth _ = Nothing


graphData :: [Text] -> (Text,[Maybe (Double, Maybe Double)]) -> [DataRow]
graphData xs (name,ys) = mapMaybe point $ zip xs ys 
  where point (x,(Just (_,Just y))) = Just
          $ object [ "Region" .= name
                   , "Date"   .= x
                   , "Growth" .= y ]
        point _ = Nothing


toFullScreenHtmlWith titleStr mopts vl =
  let spec = A.encodeToLazyText (fromVL vl)
      opts = maybe "" (\o -> "," <> A.encodeToLazyText o) mopts

  in TL.unlines
    [ "<!DOCTYPE html>"
    , "<html>"
    , "<head>"
    , "  <meta charset=\"UTF-8\">"
    , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
    , "  <title>" <> TL.fromStrict titleStr <> "</title>"
    , "  <script src=\"https://cdn.jsdelivr.net/npm/vega@5\"></script>"
    , "  <script src=\"https://cdn.jsdelivr.net/npm/vega-lite@4\"></script>"
    , "  <script src=\"https://cdn.jsdelivr.net/npm/vega-embed\"></script>"
    , "</head>"
    , "<body>"
    , "<div id=\"vis\" style=\"overflow: hidden; position: absolute;"
      <> "top: 0; left: 0; right: 0; bottom: 0;\"></div>"
    , "<script type=\"text/javascript\">"
    , "  var spec = " <> spec <> ";"
    , "  vegaEmbed(\'#vis\', spec" <> opts <> ").then(function(result) {"
    , "  // Access the Vega view instance (https://vega.github.io/vega/docs/api/view/) as result.view"
    , "  }).catch(console.error);"
    , "</script>"
    , "</body>"
    , "</html>"
    ]
