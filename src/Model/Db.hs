{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Model.Db (
    createTables
  , queryTodaysWeight) where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe (listToMaybe)
import qualified Data.Text as T
import           Data.Time (UTCTime)
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
queryTodaysWeight conn (User id _) today = do
  weights <- query conn "SELECT weight FROM weights WHERE user_id = ? AND date = date(?) LIMIT 1" (id, today)
  return $
    case weights of
      [Only f] -> Just f
      _ -> Nothing
