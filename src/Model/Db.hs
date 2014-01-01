{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Model.Db (
    createTables
  , setWeight
  , queryWeights
  , queryTodaysWeight
  , queryOptions
  , queryOptionDouble
  , addNote
  , deleteNote
  , queryTodaysNotes
  , queryWorkout
  , queryTodaysWorkouts
  , queryExercise
  , queryExercises
  , addExercise
  , createWorkout
  , queryWorkoutExerciseSets
  , addExerciseSet) where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Time (Day, UTCTime)
import           Database.SQLite.Simple

import           Model.Types

data SetRow = SetRow {
    _srId         :: RowId
  , _srTimestamp  :: UTCTime
  ,  srExerciseId :: RowId
  , _srReps       :: Int
  , _srWeight     :: Double
  , _srComment    :: Maybe T.Text
  }

instance FromRow Note where
  fromRow = Note <$> field <*> field <*> field

instance FromRow Workout where
  fromRow = Workout <$> fmap RowId field <*> field <*> field <*> pure []

instance FromRow Exercise where
  fromRow = Exercise <$> fmap RowId field <*> field

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
              , " type TEXT)"])
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
        when (version == n) $ (u conn) >> setSchemaVersion conn (n + 1)
  upgradeVersion 0 upgradeTo1
  execute_ conn "COMMIT"


setWeight :: Connection -> User -> UTCTime -> Maybe Double -> IO ()
setWeight conn (User uid _) today weight =
  maybe del ins weight
  where
    del =
      execute conn "DELETE FROM weights WHERE user_id = ? AND date = date(?)" (uid, today)
    ins v =
      execute conn "INSERT INTO weights (user_id, date, weight) VALUES(?,date(?),?)" (uid, today, v)

queryTodaysWeight :: Connection -> User -> UTCTime -> IO (Maybe Double)
queryTodaysWeight conn (User uid _) today = do
  weights <- query conn "SELECT weight FROM weights WHERE user_id = ? AND date = date(?) LIMIT 1" (uid, today)
  return $
    case weights of
      [Only f] -> Just f
      _ -> Nothing

queryWeights :: Connection -> User -> UTCTime -> Int -> IO [(Day, Double)]
queryWeights conn (User uid _) today lastNDays = do
  let days' = if lastNDays == 0 then maxBound else lastNDays
  query conn
    (Query $ T.concat [ "SELECT date,weight FROM weights WHERE (user_id = ?) AND "
                      , "((julianday(?) - julianday(date)) <= ?) "
                      , "ORDER BY date ASC"])
    (uid, today, days')

addNote :: Connection -> User -> UTCTime -> T.Text -> IO ()
addNote conn (User uid _) today note =
  execute conn "INSERT INTO notes (user_id,timestamp,comment) VALUES (?,?,?)" (uid, today, note)

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
  [e] <- query conn "SELECT id,name FROM exercises WHERE id = ? LIMIT 1" (Only . unRowId $ rowId)
  return e

queryExercises :: Connection -> IO [Exercise]
queryExercises conn = do
  query_ conn "SELECT id,name FROM exercises ORDER BY lower(name)"

addExercise :: Connection -> T.Text -> IO ()
addExercise conn name = do
  execute conn "INSERT INTO exercises (name) VALUES (?)" (Only name)

querySets :: Connection -> User -> RowId -> Maybe RowId -> IO [SetRow]
querySets conn (User uid _) wrkId exerciseId_ = do
  query conn
    (Query $
      T.concat [ "SELECT id,timestamp,exercise_id,reps,weight,comment FROM sets "
                , " WHERE (user_id = ?) AND (workout_id = ?) AND "
                , if isNothing exerciseId_ then "?" else "(exercise_id = ?)"
                , " ORDER BY id"
                ])
      (uid, unRowId wrkId, fromMaybe 1 (unRowId <$> exerciseId_))

setRowsToSets :: [SetRow] -> [ExerciseSet]
setRowsToSets =
  map (\(SetRow i ts _eid reps weight comment) -> ExerciseSet i ts reps weight comment)

createWorkout :: Connection -> User -> UTCTime -> IO RowId
createWorkout conn (User uid _) today = do
  execute conn "INSERT INTO workouts (user_id,timestamp) VALUES (?,?)" (uid, today)
  rowId <- lastInsertRowId conn
  return . RowId $ rowId

workoutExercises :: Connection  -> User -> M.Map RowId Exercise -> Workout -> IO Workout
workoutExercises conn user exercises w = do
  -- Compute sort order for exercise groups.  We don't have an
  -- explicit "UI insert order id" in the database schema so we
  -- do a bit of gymnastics here to extract the same ordering
  -- based on row ids.
  sets <- querySets conn user (workoutId w) Nothing
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
  return . M.fromList . map (\e@(Exercise i _n) -> (i, e)) $ es

queryWorkout :: Connection -> User -> RowId -> IO Workout
queryWorkout conn user@(User uid _) workoutId_ = do
  [workout] <-
    query conn
      "SELECT id,timestamp,comment FROM workouts WHERE (user_id = ?) AND (id = ?)" (uid, unRowId workoutId_)
      :: IO [Workout]
  exercises <- listExercisesMap conn
  workoutExercises conn user exercises workout

queryTodaysWorkouts :: Connection -> User -> UTCTime -> IO [Workout]
queryTodaysWorkouts conn user@(User uid _) today = do
  exercises <- listExercisesMap conn
  ws <-
    query conn
      "SELECT id,timestamp,comment FROM workouts WHERE (user_id = ?) AND (date(timestamp) = date(?))" (uid, today)
      :: IO [Workout]
  mapM (workoutExercises conn user exercises) ws

queryWorkoutExerciseSets :: Connection -> User -> RowId -> RowId -> IO [ExerciseSet]
queryWorkoutExerciseSets conn user workoutId_ exerciseId_ = do
  querySets conn user workoutId_ (Just exerciseId_) >>= return . setRowsToSets

addExerciseSet :: Connection -> User -> RowId -> RowId -> Int -> Double -> IO ()
addExerciseSet conn (User uid _) workoutId_ exerciseId_ reps weight = do
  execute conn "INSERT INTO sets (user_id,workout_id,exercise_id,reps,weight) VALUES (?,?,?,?,?)"
    (uid, unRowId workoutId_, unRowId exerciseId_, reps, weight)
