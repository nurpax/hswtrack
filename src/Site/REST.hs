{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- This module defines the JSON interface used for talking with the
-- client.  These types do not directly map to what's in the Model,
-- rather these are more coupled with client app logic.
module Site.REST
  ( AppContext(..)
  , WeightSample(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.Text as T
import           Data.Time (UTCTime, Day, formatTime)
import           System.Locale (defaultTimeLocale)

-- Everything needed for rendering the home/settings page
data AppContext = AppContext {
    acLogin :: T.Text
  , acWeight :: Maybe Float
  }

data WeightSample = WeightSample {
    wsDate :: Day
  , wsWeight :: Float
  }

instance FromJSON AppContext where
  parseJSON (Object v) =
    AppContext <$> v .: "login" <*> v .: "weight"
  parseJSON _ = mzero

instance ToJSON AppContext where
  toJSON (AppContext u w) =
    object [ "login"  .= u
           , "weight" .= w
           ]

instance ToJSON WeightSample where
  toJSON (WeightSample d w) =
    object [ "date"  .= formatTime defaultTimeLocale "%F" d
           , "weight" .= w
           ]
