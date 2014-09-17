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
  , restClearWeight
  , restListWeights
  , restAddNote
  , restDeleteNote
  , restListNotes
  , restListExerciseTypes
  , restNewExerciseType
  , restNewWorkout
  , restModifyWorkout
  , restQueryWorkouts
  , restAddExerciseSet
  , restDeleteExerciseSet
  , restQueryWorkoutHistory
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad (mzero)
import           Control.Error.Safe (tryJust)
import           Control.Monad.Trans (lift, liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson hiding (json)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import           Data.Time
import           System.Locale (defaultTimeLocale)
------------------------------------------------------------------------------
import           Model
import           Site.Util
------------------------------------------------------------------------------

type H = Handler App App

-- Everything needed for rendering the home/settings page
data AppContext = AppContext {
    acLoggedIn   :: Bool
  , acLoginError :: Maybe T.Text
  , acContext    :: Maybe LoggedInContext
  }

data LoggedInContext = LoggedInContext {
    _ctxLogin    :: T.Text
  , _ctxWeight   :: Maybe WeightSample
  , _ctxSettings :: M.Map String ConfigVal
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
  toJSON (CVDouble flt) = Number (realToFrac flt)

instance ToJSON WeightSample where
  toJSON (WeightSample i d w) =
    object [ "id"     .= i
           , "date"   .= formatTime defaultTimeLocale "%F" d
           , "weight" .= w
           ]

instance ToJSON Note where
  toJSON (Note i t n) =
    object [ "id"   .= i
           , "time" .= t
           , "text" .= n
           ]

instance ToJSON RowId where
  toJSON (RowId i) = Number (fromIntegral i)

-- Temp data type for JSON schema tweaking
data ExerciseSets = ExerciseSets Exercise [ExerciseSet]

instance ToJSON ExerciseSets where
  toJSON (ExerciseSets e es) =
    object [ "name"       .= exerciseName e
           , "id"         .= exerciseId e
           , "type"       .= (exerciseTypeToText . exerciseType $ e)
           , "sets"       .= es
           ]

instance ToJSON Workout where
  toJSON (Workout i u t c p es) =
    object [ "id"        .= i
           , "userId"    .= u
           , "time"      .= t
           , "comment"   .= c
           , "public"    .= p
           , "exercises" .= map (uncurry ExerciseSets) es
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

data WorkoutPutReq = WorkoutPutReq {
    wputWorkoutId :: RowId
  , wputPublic    :: Bool
  }

instance FromJSON WorkoutPutReq where
    parseJSON (Object v) = WorkoutPutReq <$> (RowId <$> v .: "id") <*> v .: "public"
    parseJSON _          = mzero

restLoginError :: MonadSnap m => T.Text -> m ()
restLoginError e =
  writeJSON (AppContext False (Just e) Nothing)

loginReqdResponse :: ToJSON a => (User -> EitherT HttpError H a) -> H ()
loginReqdResponse action = with auth currentUser >>= go
  where
    go Nothing  =
      modifyResponse $ setResponseStatus 403 "Login required"

    go (Just u) = runHttpErrorEitherT $ do
      uid  <- tryJust (badReq "withLoggedInUser: missing uid") (userId u)
      uid' <- hoistHttpError (reader T.decimal (unUid uid))
      let modelUser = Model.User uid' (userLogin u)
      json <- toJSON <$> action modelUser
      return . writeJSON . wrapPayload (Just modelUser) $ json

anonResponse :: ToJSON a => (Maybe User -> EitherT HttpError H a) -> H ()
anonResponse action = do
  with auth currentUser >>= \u -> runHttpErrorEitherT $ do
    go u
  where
    go Nothing  = do
      p <- action Nothing
      return . writeJSON $ wrapPayload Nothing (toJSON p)

    go (Just u) = do
      uid  <- tryJust (badReq "withLoggedInUser: missing uid") (userId u)
      uid' <- hoistHttpError (reader T.decimal (unUid uid))
      let modelUser = Just $ Model.User uid' (userLogin u)
      p <- action modelUser
      return . writeJSON $ wrapPayload modelUser (toJSON p)

-- Get requested date either from GET params or return today's time if not specified.
-- FIXME: at some point we need to decide how to deal with timezones here
getToday :: EitherT HttpError H UTCTime
getToday = do
  today <- maybeGetTextParam "date"
  case today of
    Just t  -> do
      let t' = parseTime defaultTimeLocale "%Y-%m-%d" . T.unpack $ t
      tryJust (badReq "invalid GET date format") t'
    Nothing -> liftIO getCurrentTime

-- Every page render calls this handler to get an "app context".  This
-- context struct contains things like is the user logged in, what's
-- his name, etc.  This is used on client-side to implement login
-- screen, among other things.
restAppContext :: H ()
restAppContext = loginReqdResponse get
  where
    get user@(Model.User _ login) = do
      today <- getToday
      (weight, options) <-
        lift $ withDb $ \conn -> do
          weight <- Model.queryTodaysWeight conn user today
          options <- Model.queryOptions conn user
          return (weight, options)
      return $ AppContext True Nothing (Just (LoggedInContext login weight options))

restClearWeight :: H ()
restClearWeight = loginReqdResponse clear
  where
    clear user = do
      weightId <- RowId <$> getInt64Param "id"
      lift $ withDb $ \conn -> Model.deleteWeight conn user weightId

restSetWeight :: H ()
restSetWeight = loginReqdResponse set
  where
    set user = do
      today  <- getToday
      weight <- getDoubleParam "weight"
      lift $ withDb $ \conn -> do
        Model.setWeight conn user today weight

restListWeights :: H ()
restListWeights =
  loginReqdResponse $ \user -> do
    today     <- getToday
    lastNDays <- getIntParam "days"
    lift $ withDb $ \conn -> Model.queryWeights conn user today lastNDays

restAddNote :: H ()
restAddNote = loginReqdResponse get
  where
    get user = do
      today    <- getToday
      noteText <- getTextParam "text"
      lift $ withDb $ \conn -> Model.addNote conn user today noteText

restDeleteNote :: H ()
restDeleteNote = loginReqdResponse get
  where
    get user = do
      noteId <- getIntParam "id"
      lift $ withDb $ \conn -> Model.deleteNote conn user noteId

restListNotes :: H ()
restListNotes = loginReqdResponse $ \user -> do
  today <- getToday
  lift $ withDb $ \conn -> Model.queryTodaysNotes conn user today

----------------------------------------------------------------------
-- Workout related AJAX entry points

wrapPayload :: Maybe Model.User -> Value -> Value
wrapPayload Nothing v =
  object [ "loggedIn"  .= False
         , "payload"   .= v
         ]
wrapPayload (Just (Model.User uid login)) v =
  object [ "loggedIn"  .= True
         , "userId"    .= uid
         , "userLogin" .= login
         , "payload"   .= v
         ]

restListExerciseTypes :: H ()
restListExerciseTypes = anonResponse $ \_user -> do
  lift $ withDb $ \conn -> Model.queryExercises conn

-- TODO need to check for dupes by lower case name here, and return
-- error if already exists
restNewExerciseType :: H ()
restNewExerciseType = loginReqdResponse $ \_user -> do
  name <- getTextParam "name"
  ty   <- getTextParam "type" >>= hoistHttpError . textToExerciseType
  lift $ withDb $ \conn ->
    Model.addExercise conn name ty

restQueryWorkouts :: H ()
restQueryWorkouts = do
  wrkId <- getParam "id"
  maybe (loginReqdResponse listWorkouts) (anonResponse . oneWorkout) wrkId
  where
    oneWorkout id_ user = do
      workoutId_ <- parseInt64 . T.decodeUtf8 $ id_
      workout <-
        lift $ withDb $ \conn -> Model.queryWorkout conn user (RowId workoutId_)
      tryJust (forbiddenReq "missing or unauthorized workout access") workout

    listWorkouts user = do
      today <- getToday
      lift $ withDb $ \conn -> Model.queryTodaysWorkouts conn user today

restNewWorkout :: H ()
restNewWorkout = loginReqdResponse new
  where
    new user = do
      today <- getToday
      lift $ withDb $ \conn -> Model.createWorkout conn user today

restAddExerciseSet :: H ()
restAddExerciseSet = loginReqdResponse put
  where
    put user = do
      workoutId_  <- RowId <$> getInt64Param "workoutId"
      exerciseId_ <- RowId <$> getInt64Param "exerciseId"
      reps        <- getIntParam "reps"
      weight      <- getDoubleParam "weight"
      lift $ withDb $ \conn ->
        Model.addExerciseSet conn user workoutId_ exerciseId_ reps weight

restDeleteExerciseSet :: H ()
restDeleteExerciseSet = loginReqdResponse del
  where
    del user = do
      setId_ <- RowId <$> getInt64Param "id"
      lift $ withDb $ \conn ->
        Model.deleteExerciseSet conn user setId_

restQueryWorkoutHistory :: H ()
restQueryWorkoutHistory = loginReqdResponse $ \user -> do
  limit <- getIntParam "limit"
  today <- getToday
  lift $ withDb $ \conn -> Model.queryPastWorkouts conn user today limit

restModifyWorkout :: H ()
restModifyWorkout = loginReqdResponse modify
  where
    modify user = do
      reqParams' <- lift getJSON
      parms <- hoistHttpError reqParams'
      lift $ withDb $ \conn -> do
        Model.makeWorkoutPublic conn user (wputWorkoutId parms) (wputPublic parms)
        Model.queryWorkout conn (Just user) (wputWorkoutId parms)
