{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- This module defines the JSON interface used for talking with the
-- client.  These types do not directly map to what's in the Model,
-- rather these are more coupled with client app logic.
module Site.REST
  ( AppContext(..)
  , ConfigVal(..)
  , WeightSample(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Attoparsec.Number (Number(D))
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Time (Day, formatTime)
import           System.Locale (defaultTimeLocale)

import           Model

-- Everything needed for rendering the home/settings page
data AppContext = AppContext {
    acLogin :: T.Text
  , acWeight :: Maybe Double
  , acSettings :: M.Map String ConfigVal
  }

data WeightSample = WeightSample {
    wsDate :: Day
  , wsWeight :: Double
  }

instance FromJSON AppContext where
  parseJSON (Object v) =
    AppContext <$> v .: "login" <*> v .: "weight" <*> (pure M.empty)
  parseJSON _ = mzero

instance ToJSON AppContext where
  toJSON (AppContext u w o) =
    object [ "login"   .= u
           , "weight"  .= w
           , "options" .= o
           ]

instance ToJSON ConfigVal where
  toJSON (CVString txt) = String txt
  toJSON (CVDouble flt) = Number (D flt)

instance ToJSON WeightSample where
  toJSON (WeightSample d w) =
    object [ "date"  .= formatTime defaultTimeLocale "%F" d
           , "weight" .= w
           ]
