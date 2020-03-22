module Series (Series(..), Item(..), DataMap) where

import Data.Map (Map)
import Data.Text (Text)


data Series = Series { serRegion :: Text
                     , serDates :: [Text]
                     , serValues :: [Int]
                     } deriving Show


data Item = Confirmed | Active | Recovered | Deaths
            deriving (Eq,Show)

type DataMap = Map Text Series
