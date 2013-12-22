{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- This module defines the JSON interface used for talking with the
-- client.  These types do not directly map to what's in the Model,
-- rather these are more coupled with client app logic.
module Site.REST
  ( AppContext(..)
  , ConfigVal(..)
  , WeightSample(..)
  , restAppContext
  , restLoginError
  , restSetWeight
  , restListWeights
  , restAddNote
  , restDeleteNote
  , restListNotes
  ) where

------------------------------------------------------------------------------
import           Control.Error.Safe (tryJust)
import           Control.Monad.Trans (lift, liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Attoparsec.Number (Number(D))
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as T
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
    acLoggedIn :: Bool
  , acLoginError :: Maybe T.Text
  , acContext :: Maybe LoggedInContext
  }

data LoggedInContext = LoggedInContext {
    _ctxLogin :: T.Text
  , _ctxWeight :: Maybe Double
  , _ctxSettings :: M.Map String ConfigVal
  }

data WeightSample = WeightSample {
    wsDate :: Day
  , wsWeight :: Double
  }

instance ToJSON AppContext where
  toJSON (AppContext loggedIn e ctx) =
    object [ "loggedIn"   .= loggedIn
           , "loginError" .= e
           , "context"    .= ctx
           ]

instance ToJSON LoggedInContext where
  toJSON (LoggedInContext u w o) =
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

instance ToJSON Note where
  toJSON (Note i t n) =
    object [ "id"   .= i
           , "time" .= t
           , "text" .= n
           ]

-- | Run actions with a logged in user or go back to the login screen
requireLoggedInUser :: (Model.User -> H ()) -> H ()
requireLoggedInUser action =
  with auth currentUser >>= go
  where
    go Nothing  = do
      modifyResponse $ setResponseStatus 403 "Login required"

    go (Just u) = logRunEitherT $ do
      uid  <- tryJust "withLoggedInUser: missing uid" (userId u)
      uid' <- hoistEither (reader T.decimal (unUid uid))
      return $ action (Model.User uid' (userLogin u))

-- Every page render calls this handler to get an "app context".  This
-- context struct contains things like is the user logged in, what's
-- his name, etc.  This is used on client-side to implement login
-- screen, among other things.
restAppContext :: H ()
restAppContext =
  with auth currentUser >>= tryLogin
  where
    tryLogin Nothing =
      writeJSON (AppContext False Nothing Nothing)

    tryLogin (Just u) = logRunEitherT $ do
      uid  <- tryJust "withLoggedInUser: missing uid" (userId u)
      uid' <- hoistEither (reader T.decimal (unUid uid))
      return $ get (Model.User uid' (userLogin u))

    get user@(Model.User _ login)  = do
      today <- liftIO $ getCurrentTime
      (weight, options) <-
        withDb $ \conn -> do
          weight <- Model.queryTodaysWeight conn user today
          options <- Model.queryOptions conn user
          return (weight, options)
      let appContext = AppContext True Nothing (Just (LoggedInContext login weight options))
      writeJSON appContext

restLoginError :: MonadSnap m => T.Text -> m ()
restLoginError e =
  writeJSON (AppContext False (Just e) Nothing)

restSetWeight :: H ()
restSetWeight =
  method POST (requireLoggedInUser get)
  where
    get user  = logRunEitherT $ do
      today  <- liftIO $ getCurrentTime
      weight <- getDoubleParamOrEmpty "weight"
      lift $ withDb $ \conn -> Model.setWeight conn user today weight
      return . writeJSON $ (1 :: Int)

restListWeights :: H ()
restListWeights =
  method GET (requireLoggedInUser get)
  where
    get user  = logRunEitherT $ do
      today     <- liftIO $ getCurrentTime
      lastNDays <- getIntParam "days"
      weights   <- lift $ withDb $ \conn -> Model.queryWeights conn user today lastNDays
      return . writeJSON $ map (\(d,w) -> WeightSample d w) weights

restAddNote :: H ()
restAddNote =
  requireLoggedInUser get
  where
    get user  = logRunEitherT $ do
      today    <- liftIO $ getCurrentTime
      noteText <- getTextParam "text"
      notes <- lift $ withDb $ \conn -> do
        Model.addNote conn user today noteText
        Model.queryTodaysNotes conn user today
      return . writeJSON $ notes

restDeleteNote :: H ()
restDeleteNote =
  requireLoggedInUser get
  where
    get user  = logRunEitherT $ do
      today  <- liftIO $ getCurrentTime
      noteId <- getIntParam "id"
      notes <- lift $ withDb $ \conn -> do
        Model.deleteNote conn user noteId
        Model.queryTodaysNotes conn user today
      return . writeJSON $ notes

restListNotes :: H ()
restListNotes =
  method GET (requireLoggedInUser get)
  where
    get user  = logRunEitherT $ do
      today <- liftIO $ getCurrentTime
      notes <- lift $ withDb $ \conn -> Model.queryTodaysNotes conn user today
      return . writeJSON $ notes
