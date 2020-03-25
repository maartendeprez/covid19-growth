{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Series (Series(..), Item(..), DataMap) where

import GHC.Generics

import Data.Time.Calendar

import Data.Map (Map)
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)

data Series = Series { serRegion :: Text
                     , serDates :: [Day]
                     , serValues :: [Int]
                     } deriving (Show,Generic,FromJSON,ToJSON)

data Item = Confirmed | Active | Recovered | Deaths
            deriving (Eq,Ord,Show,Generic,FromJSON,ToJSON,FromJSONKey,ToJSONKey)

type DataMap = Map Text Series
