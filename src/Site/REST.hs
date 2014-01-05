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
  , restListExerciseTypes
  , restNewExerciseType
  , restNewWorkout
  , restQueryWorkouts
  , restAddExerciseSet
  ) where

------------------------------------------------------------------------------
import           Control.Error.Safe (tryJust)
import           Control.Monad.Trans (lift, liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Attoparsec.Number (Number(D, I))
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

instance ToJSON RowId where
  toJSON (RowId i) = Number (I (fromIntegral i))

-- Temp data type for JSON schema tweaking
data ExerciseSets = ExerciseSets Exercise [ExerciseSet]

instance ToJSON ExerciseSets where
  toJSON (ExerciseSets e es) =
    object [ "name"       .= exerciseName e
           , "exerciseId" .= exerciseId e
           , "sets"       .= es
           ]

instance ToJSON Workout where
  toJSON (Workout i t c es) =
    object [ "id"        .= i
           , "time"      .= t
           , "comment"   .= c
           , "exercises" .= map (\(e,ess) -> ExerciseSets e ess) es
           ]

instance ToJSON Exercise where
  toJSON (Exercise i n t) =
    object [ "id"   .= i
           , "name" .= n
           , "type" .= exerciseTypeToText t
           ]

instance ToJSON ExerciseSet where
  toJSON (ExerciseSet i ts reps weight comment) =
    object [ "id"      .= i
           , "time"    .= ts
           , "reps"    .= reps
           , "weight"  .= weight
           , "comment" .= comment
           ]

restLoginError :: MonadSnap m => T.Text -> m ()
restLoginError e =
  writeJSON (AppContext False (Just e) Nothing)

-- | Run actions with a logged in user or go back to the login screen
requireLoggedInUser :: (Model.User -> EitherT String H (H ())) -> H ()
requireLoggedInUser action =
  with auth currentUser >>= go
  where
    go Nothing  = do
      modifyResponse $ setResponseStatus 403 "Login required"

    go (Just u) = logRunEitherT $ do
      uid  <- tryJust "withLoggedInUser: missing uid" (userId u)
      uid' <- hoistEither (reader T.decimal (unUid uid))
      action (Model.User uid' (userLogin u))


jsonResponse :: ToJSON a => (Model.User -> EitherT String H a) -> H ()
jsonResponse action = requireLoggedInUser respond
  where
    respond user = (action user >>= return . writeJSON)

-- Every page render calls this handler to get an "app context".  This
-- context struct contains things like is the user logged in, what's
-- his name, etc.  This is used on client-side to implement login
-- screen, among other things.
restAppContext :: H ()
restAppContext = jsonResponse get
  where
    get user@(Model.User _ login) = do
      today <- liftIO $ getCurrentTime
      (weight, options) <-
        lift $ withDb $ \conn -> do
          weight <- Model.queryTodaysWeight conn user today
          options <- Model.queryOptions conn user
          return (weight, options)
      return $ AppContext True Nothing (Just (LoggedInContext login weight options))

restSetWeight :: H ()
restSetWeight =
  method POST (jsonResponse get)
  where
    get user = do
      today  <- liftIO $ getCurrentTime
      weight <- getDoubleParamOrEmpty "weight"
      lift $ withDb $ \conn -> Model.setWeight conn user today weight
      return (1 :: Int)

restListWeights :: H ()
restListWeights = method GET (jsonResponse get)
  where
    get user = do
      today     <- liftIO $ getCurrentTime
      lastNDays <- getIntParam "days"
      weights   <- lift $ withDb $ \conn -> Model.queryWeights conn user today lastNDays
      return . map (\(d,w) -> WeightSample d w) $ weights

restAddNote :: H ()
restAddNote = jsonResponse get
  where
    get user = do
      today    <- liftIO $ getCurrentTime
      noteText <- getTextParam "text"
      lift $ withDb $ \conn -> do
        Model.addNote conn user today noteText
        Model.queryTodaysNotes conn user today

restDeleteNote :: H ()
restDeleteNote = jsonResponse get
  where
    get user = do
      today  <- liftIO $ getCurrentTime
      noteId <- getIntParam "id"
      lift $ withDb $ \conn -> do
        Model.deleteNote conn user noteId
        Model.queryTodaysNotes conn user today

restListNotes :: H ()
restListNotes = method GET (jsonResponse get)
  where
    get user = do
      today <- liftIO $ getCurrentTime
      lift $ withDb $ \conn -> Model.queryTodaysNotes conn user today

----------------------------------------------------------------------
-- Workout related AJAX entry points

restListExerciseTypes :: H ()
restListExerciseTypes = jsonResponse get
  where
    get _user = do
      lift $ withDb $ \conn -> Model.queryExercises conn


-- TODO need to check for dupes by lower case name here, and return
-- error if already exists
restNewExerciseType :: H ()
restNewExerciseType = jsonResponse put
  where
    put _user = do
      name <- getTextParam "name"
      ty   <- getTextParam "type" >>= hoistEither . textToExerciseType
      lift $ withDb $ \conn -> do
        Model.addExercise conn name ty
        Model.queryExercises conn

restQueryWorkouts :: H ()
restQueryWorkouts = do
  wrkId <- getParam "id"
  maybe (jsonResponse listWorkouts) (\i -> jsonResponse (oneWorkout i)) wrkId
  where
    oneWorkout id_ user = do
      workoutId_ <- parseInt64 . T.decodeUtf8 $ id_
      lift $ withDb $ \conn -> Model.queryWorkout conn user (RowId workoutId_)

    listWorkouts user = do
      today <- liftIO $ getCurrentTime
      lift $ withDb $ \conn -> Model.queryTodaysWorkouts conn user today

restNewWorkout :: H ()
restNewWorkout = jsonResponse new
  where
    new user = do
      today <- liftIO $ getCurrentTime
      lift $ withDb $ \conn -> do
        Model.createWorkout conn user today
        Model.queryTodaysWorkouts conn user today

restAddExerciseSet :: H ()
restAddExerciseSet = jsonResponse get
  where
    get user = do
      workoutId_  <- fmap RowId $ getInt64Param "workoutId"
      exerciseId_ <- fmap RowId $ getInt64Param $ "exerciseId"
      reps        <- getIntParam "reps"
      weight      <- getDoubleParam "weight"
      lift $ withDb $ \conn -> do
        Model.addExerciseSet conn user workoutId_ exerciseId_ reps weight
        sets     <- Model.queryWorkoutExerciseSets conn user workoutId_ exerciseId_
        exercise <- Model.queryExercise conn exerciseId_
        return (ExerciseSets exercise sets)
