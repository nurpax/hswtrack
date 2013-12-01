{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Model.Db (
    createTables
  , setWeight
  , queryWeights
  , queryTodaysWeight
  , queryOptions
  , queryOptionDouble) where

import           Control.Monad
import           Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Time (Day, UTCTime)
import           Database.SQLite.Simple

import           Model.Types

tableExists :: Connection -> String -> IO Bool
tableExists conn tblName = do
  r <- query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
  case r of
    [Only (_ :: String)] -> return True
    _ -> return False

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

queryOptions :: Connection -> User -> IO (M.Map String ConfigVal)
queryOptions conn user = do
  minWeight <- queryOptionDouble conn user "min_graph_weight"
  let options = maybeToList (fmap (\w -> ("minGraphWeight", CVDouble w)) minWeight)
  return . M.fromList $ options


queryOptionDouble :: Connection -> User -> T.Text -> IO (Maybe Double)
queryOptionDouble conn (User uid _) optionName = do
  v <- query conn "SELECT option_value FROM user_options WHERE user_id = ? AND option_name = ? LIMIT 1" (uid, optionName)
  return . fmap (\(Only x) -> read x) . listToMaybe $ v
