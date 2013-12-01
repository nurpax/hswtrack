{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- This module defines the JSON interface used for talking with the
-- client.  These types do not directly map to what's in the Model,
-- rather these are more coupled with client app logic.
module Site.REST
  ( AppContext(..)
  , ConfigVal(..)
  , WeightSample(..)
  , restAppContext
  , restSetWeight
  , restListWeights
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans (lift, liftIO)
import           Data.Aeson
import           Data.Attoparsec.Number (Number(D))
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Time
import           Snap.Extras.JSON
import           System.Locale (defaultTimeLocale)
------------------------------------------------------------------------------
import           Model
import           Site.Util
------------------------------------------------------------------------------

type H = Handler App App

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

restAppContext :: H ()
restAppContext =
  method GET (withLoggedInUser get)
  where
    get user@(Model.User _ login)  = do
      today <- liftIO $ getCurrentTime
      (weight, options) <-
        withDb $ \conn -> do
          weight <- Model.queryTodaysWeight conn user today
          options <- Model.queryOptions conn user
          return (weight, options)
      let appContext = AppContext login weight options
      writeJSON appContext

restSetWeight :: H ()
restSetWeight =
  method POST (withLoggedInUser get)
  where
    get user  = logRunEitherT $ do
      today  <- liftIO $ getCurrentTime
      weight <- getDoubleParamOrEmpty "weight"
      lift $ withDb $ \conn -> Model.setWeight conn user today weight
      return . writeJSON $ (1 :: Int)

restListWeights :: H ()
restListWeights =
  method GET (withLoggedInUser get)
  where
    get user  = logRunEitherT $ do
      today     <- liftIO $ getCurrentTime
      lastNDays <- getIntParam "days"
      weights   <- lift $ withDb $ \conn -> Model.queryWeights conn user today lastNDays
      return . writeJSON $ map (\(d,w) -> WeightSample d w) weights

