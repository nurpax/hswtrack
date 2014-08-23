{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Model.Db (
    createTables
  , setWeight
  , deleteWeight
  , queryWeights
  , queryTodaysWeight
  , queryOptions
  , queryOptionDouble
  , addNote
  , deleteNote
  , queryTodaysNotes
  , queryWorkout
  , queryTodaysWorkouts
  , queryPastWorkouts
  , queryExercise
  , queryExercises
  , addExercise
  , createWorkout
  , makeWorkoutPublic
  , addExerciseSet
  , deleteExerciseSet) where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.ToField

import           Model.Types

data SetRow = SetRow {
    _srId         :: RowId
  , _srTimestamp  :: UTCTime
  ,  srExerciseId :: RowId
  , _srReps       :: Int
  , _srWeight     :: Double
  , _srComment    :: Maybe T.Text
  }

instance FromRow WeightSample where
  fromRow = WeightSample <$> fmap RowId field <*> field <*> field

instance FromRow Note where
  fromRow = Note <$> field <*> field <*> field

instance FromRow Workout where
  fromRow = Workout <$> fmap RowId field <*> fmap RowId field <*> field <*> field <*> field <*> pure []

instance ToField ExerciseType where
  toField e = SQLText . exerciseTypeToText $ e

instance FromRow Exercise where
  fromRow =
    Exercise <$> fmap RowId field <*> field <*> fmap (cvt . textToExerciseType) field
    where
      cvt (Left v)  = error v -- SQL table constraints should prevent this from happening
      cvt (Right v) = v

instance FromRow SetRow where
  fromRow = SetRow <$> fmap RowId field <*> field <*> fmap RowId field <*> field <*> field <*> field

tableExists :: Connection -> String -> IO Bool
tableExists conn tblName = do
  r <- query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
  case r of
    [Only (_ :: String)] -> return True
    _ -> return False

schemaVersion :: Connection -> IO Int
schemaVersion conn = do
  [Only v] <- query_ conn "SELECT version FROM version"
  return v

setSchemaVersion :: Connection -> Int -> IO ()
setSchemaVersion conn ver =
  execute conn "UPDATE version SET version = ?" (Only ver)

upgradeTo1 :: Connection -> IO ()
upgradeTo1 conn = do
  execute_ conn
    (Query $
     T.concat [ "CREATE TABLE workouts ("
              , " id INTEGER PRIMARY KEY,"
              , " timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,"
              , " user_id INTEGER NOT NULL,"
              , " comment TEXT)"])
  execute_ conn
    (Query $
     T.concat [ "CREATE TABLE exercises ("
              , " id INTEGER PRIMARY KEY,"
              , " name TEXT,"
              , " type TEXT NOT NULL DEFAULT 'W' CHECK(type IN ('W', 'BW')))"])
  execute_ conn
    (Query $
     T.concat [ "CREATE TABLE sets ("
              , " id INTEGER PRIMARY KEY,"
              , " timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,"
              , " user_id INTEGER,"
              , " workout_id INTEGER,"
              , " exercise_id INTEGER,"
              , " reps INTEGER,"
              , " weight REAL,"
              , " comment TEXT)"])

upgradeTo2 :: Connection -> IO ()
upgradeTo2 conn = do
  execute_ conn "ALTER TABLE workouts ADD COLUMN public BOOL DEFAULT 0 NOT NULL"

-- | Create the necessary database tables, if not already initialized.
createTables :: Connection -> IO ()
createTables conn = do
  -- Note: for a bigger app, you probably want to create a 'version'
  -- table too and use it to keep track of schema version and
  -- implement your schema upgrade procedure here.
  schemaCreated <- tableExists conn "weights"
  unless schemaCreated $ do
    execute_ conn
      (Query $
       T.concat [ "CREATE TABLE notes ("
                , "  id INTEGER PRIMARY KEY,"
                , "  timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,"
                , "  user_id INTEGER NOT NULL,"
                , "  comment TEXT);"])
    execute_ conn
      (Query $
       T.concat [ "CREATE TABLE user_options ("
                , "  user_id INTEGER,"
                , "  option_name TEXT,"
                , "  option_value TEXT);"])
    execute_ conn
      (Query $
       T.concat [ "CREATE TABLE weights ("
                , "    id INTEGER PRIMARY KEY,"
                , "    date date NOT NULL,"
                , "    user_id INTEGER,"
                , "    weight FLOAT NOT NULL"
                , ");"])
  -- Upgrades
  execute_ conn "BEGIN"
  versionExists <- tableExists conn "version"
  unless versionExists $ do
    execute_ conn "CREATE TABLE version (version INTEGER)"
    execute_ conn "INSERT INTO version (version) VALUES (0)"
  let upgradeVersion n u = do
        version <- schemaVersion conn
        when (version == n) $ u conn >> setSchemaVersion conn (n + 1)
  upgradeVersion 0 upgradeTo1
  upgradeVersion 1 upgradeTo2
  execute_ conn "COMMIT"

setWeight :: Connection -> User -> UTCTime -> Double -> IO WeightSample
setWeight conn (User uid _) today v = do
  execute conn "INSERT INTO weights (user_id, date, weight) VALUES(?,date(?),?)" (uid, today, v)
  rowId <- lastInsertRowId conn
  [r] <- query conn "SELECT id,datetime(date),weight FROM weights WHERE user_id = ? AND id = ? LIMIT 1" (uid, rowId)
  return r

deleteWeight :: Connection -> User -> RowId -> IO ()
deleteWeight conn (User uid _) (RowId rowId) =
  execute conn "DELETE FROM weights WHERE user_id = ? AND id = ?" (uid, rowId)

queryTodaysWeight :: Connection -> User -> UTCTime -> IO (Maybe WeightSample)
queryTodaysWeight conn (User uid _) today = do
  w <- query conn "SELECT id,datetime(date),weight FROM weights WHERE user_id = ? AND date = date(?) LIMIT 1" (uid, today)
  return $
    case w of
      [f] -> Just f
      _ -> Nothing

queryWeights :: Connection -> User -> UTCTime -> Int -> IO [WeightSample]
queryWeights conn (User uid _) today lastNDays = do
  let days' = if lastNDays == 0 then maxBound else lastNDays
  query conn
    (Query $ T.concat [ "SELECT id,datetime(date),weight FROM weights WHERE (user_id = ?) AND "
                      , "((julianday(?) - julianday(date)) <= ?) "
                      , "ORDER BY date ASC"])
    (uid, today, days')

addNote :: Connection -> User -> UTCTime -> T.Text -> IO Note
addNote conn (User uid _) today note = do
  execute conn "INSERT INTO notes (user_id,timestamp,comment) VALUES (?,?,?)" (uid, today, note)
  rowId <- lastInsertRowId conn
  [n] <- query conn "SELECT id,timestamp,comment FROM notes WHERE id = ?" (Only rowId)
  return n

deleteNote :: Connection -> User -> Int -> IO ()
deleteNote conn (User uid _) noteId =
  execute conn "DELETE FROM notes WHERE user_id = ? AND id = ?" (uid, noteId)

queryTodaysNotes :: Connection -> User -> UTCTime -> IO [Note]
queryTodaysNotes conn (User uid _) today =
  query conn "SELECT id,timestamp,comment FROM notes WHERE (user_id = ?) AND (date(timestamp) = date(?))" (uid, today)

queryOptions :: Connection -> User -> IO (M.Map String ConfigVal)
queryOptions conn user = do
  minWeight <- queryOptionDouble conn user "min_graph_weight"
  let options = maybeToList (fmap (\w -> ("minGraphWeight", CVDouble w)) minWeight)
  return . M.fromList $ options


queryOptionDouble :: Connection -> User -> T.Text -> IO (Maybe Double)
queryOptionDouble conn (User uid _) optionName = do
  v <- query conn "SELECT option_value FROM user_options WHERE user_id = ? AND option_name = ? LIMIT 1" (uid, optionName)
  return . fmap (\(Only x) -> read x) . listToMaybe $ v

----------------------------------------------------------------------
-- Query functions for workouts

queryExercise :: Connection -> RowId -> IO Exercise
queryExercise conn rowId = do
  [e] <- query conn "SELECT id,name,type FROM exercises WHERE id = ? LIMIT 1" (Only . unRowId $ rowId)
  return e

queryExercises :: Connection -> IO [Exercise]
queryExercises conn =
  query_ conn "SELECT id,name,type FROM exercises"

addExercise :: Connection -> T.Text -> ExerciseType -> IO Exercise
addExercise conn name ty = do
  execute conn "INSERT INTO exercises (name,type) VALUES (?,?)" (name, ty)
  eid <- lastInsertRowId conn
  queryExercise conn (RowId eid)

querySets :: Connection -> RowId -> IO [SetRow]
querySets conn wrkId =
  query conn
    (Query $
      T.concat [ "SELECT id,timestamp,exercise_id,reps,weight,comment FROM sets "
                , " WHERE workout_id = ? "
                , " ORDER BY id"
                ])
      (Only . unRowId $ wrkId)

setRowToSet :: SetRow -> ExerciseSet
setRowToSet (SetRow i ts _eid reps weight comment) = ExerciseSet i ts reps weight comment

setRowsToSets :: [SetRow] -> [ExerciseSet]
setRowsToSets  = map setRowToSet

createWorkout :: Connection -> User -> UTCTime -> IO Workout
createWorkout conn (User uid _) today = do
  execute conn "INSERT INTO workouts (user_id,timestamp) VALUES (?,?)" (uid, today)
  rowId <- lastInsertRowId conn
  [w] <- query conn "SELECT id,user_id,timestamp,comment,public FROM workouts WHERE id = ? LIMIT 1" (Only rowId)
  return w

makeWorkoutPublic :: Connection -> User -> RowId -> Bool -> IO ()
makeWorkoutPublic conn (User uid _) workoutId_ public =
  executeNamed conn "UPDATE workouts SET public = :pub WHERE id = :wid AND user_id = :uid"
    [ ":pub" := public
    , ":wid" := unRowId workoutId_
    , ":uid" := uid
    ]

workoutExercises :: Connection  -> M.Map RowId Exercise -> Workout -> IO Workout
workoutExercises conn exercises w = do
  -- Compute sort order for exercise groups.  We don't have an
  -- explicit "UI insert order id" in the database schema so we
  -- do a bit of gymnastics here to extract the same ordering
  -- based on row ids.
  sets <- querySets conn (workoutId w)
  let exerciseOrder =
        foldl (\acc e -> if srExerciseId e `notElem` acc then acc ++ [srExerciseId e] else acc) [] sets
  let sets' =
        map (\exerciseId_ ->
              let ex = exercises M.! exerciseId_ in
              (ex, setRowsToSets . filter (\r -> srExerciseId r == exerciseId_) $ sets))
          exerciseOrder
  return $ w { workoutSets = sets' }

listExercisesMap :: Connection -> IO (M.Map RowId Exercise)
listExercisesMap conn = do
  es <- queryExercises conn
  return . M.fromList . map (\e@(Exercise i _ _) -> (i, e)) $ es

-- Query a workout by workout ID.  If given a Nothing user, this query
-- will try to find a publicly shareable workout for the given ID.
queryWorkout :: Connection -> Maybe User -> RowId -> IO (Maybe Workout)
queryWorkout conn user workoutId_ = do
  ws <-
    case user of
      Just (User uid _) ->
        query conn
          "SELECT id,user_id,timestamp,comment,public FROM workouts WHERE ((user_id = ?) OR (public = 1)) AND (id = ?) LIMIT 1"
          (uid, unRowId workoutId_) :: IO [Workout]
      Nothing ->
        query conn
          "SELECT id,user_id,timestamp,comment,public FROM workouts WHERE (id = ?) AND (public = 1) LIMIT 1"
          (Only . unRowId $ workoutId_) :: IO [Workout]
  case ws of
    []  -> return Nothing
    [w] -> do
      exercises <- listExercisesMap conn
      Just <$> workoutExercises conn exercises w
    _   -> error "impossible"

queryTodaysWorkouts :: Connection -> User -> UTCTime -> IO [Workout]
queryTodaysWorkouts conn (User uid _) today = do
  exercises <- listExercisesMap conn
  ws <-
    query conn
      "SELECT id,user_id,timestamp,comment,public FROM workouts WHERE (user_id = ?) AND (date(timestamp) = date(?))" (uid, today)
      :: IO [Workout]
  mapM (workoutExercises conn exercises) ws

addExerciseSet :: Connection -> User -> RowId -> RowId -> Int -> Double -> IO ExerciseSet
addExerciseSet conn (User uid _) workoutId_ exerciseId_ reps weight = do
  execute conn "INSERT INTO sets (user_id,workout_id,exercise_id,reps,weight) VALUES (?,?,?,?,?)"
    (uid, unRowId workoutId_, unRowId exerciseId_, reps, weight)
  sid <- lastInsertRowId conn
  [r] <- query conn "SELECT id,timestamp,exercise_id,reps,weight,comment FROM sets WHERE id = ? LIMIT 1"
    (Only sid)
  return $ setRowToSet r

deleteExerciseSet :: Connection -> User -> RowId -> IO ()
deleteExerciseSet conn (User uid _) setId_ =
  execute conn "DELETE FROM sets WHERE user_id = ? AND id = ?" (uid, unRowId setId_)

-- Query N past workouts before or up to 'today'
queryPastWorkouts :: Connection -> User -> UTCTime -> Int -> IO [Workout]
queryPastWorkouts conn (User uid _) today limit = do
  exercises <- listExercisesMap conn
  ws <-
    query conn
      "SELECT id,user_id,timestamp,comment,public FROM workouts WHERE (user_id = ?) AND timestamp <= ? ORDER BY timestamp DESC LIMIT ?"
      (uid, today, limit)
      :: IO [Workout]
  -- TODO this will issue a large # of SQL queries.  If that ever
  -- becomes a problem, turn this into a join.
  mapM (workoutExercises conn exercises) ws
