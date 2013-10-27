{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Model.Db (
    createTables
  , queryWeights
  , queryTodaysWeight) where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Data.Time (Day, UTCTime, parseTime)
import           Database.SQLite.Simple
import           System.Locale (defaultTimeLocale)

import           Model.Types

data DayWeight = DayWeight { _day :: Day, _weight :: Double }

dayFromString :: String -> Day
dayFromString = fromJust . parseTime defaultTimeLocale "%F"

instance FromRow DayWeight where
  fromRow = DayWeight <$> (dayFromString <$> field) <*> field

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
                , "  user_id INTEGER PRIMARY KEY,"
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

queryTodaysWeight :: Connection -> User -> UTCTime -> IO (Maybe Double)
queryTodaysWeight conn (User uid _) today = do
  weights <- query conn "SELECT weight FROM weights WHERE user_id = ? AND date = date(?) LIMIT 1" (uid, today)
  return $
    case weights of
      [Only f] -> Just f
      _ -> Nothing

queryWeights :: Connection -> User -> IO [(Day, Double)]
queryWeights conn (User uid _) = do
  ws <- query conn "SELECT date,weight FROM weights WHERE user_id = ? ORDER BY date ASC" (Only uid)
  return $ map (\(DayWeight d w) -> (d, w)) ws
