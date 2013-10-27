{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- This module defines the JSON interface used for talking with the
-- client.  These types do not directly map to what's in the Model,
-- rather these are more coupled with client app logic.
module Site.REST
  ( AppContext(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.Text as T

-- Everything needed for rendering the home/settings page
data AppContext = AppContext {
    acLogin :: T.Text
  , acWeight :: Maybe Double
  } deriving (Show, Eq)

instance FromJSON AppContext where
  parseJSON (Object v) =
    AppContext <$> v .: "login" <*> v .: "weight"
  parseJSON _ = mzero

instance ToJSON AppContext where
  toJSON (AppContext u w) =
    object [ "login"  .= u
           , "weight" .= w
           ]
