{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Model.Db (
    createTables
  , saveComment
  , listComments) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Text as T
import           Database.SQLite.Simple

import           Model.Types

instance FromRow Comment where
  fromRow = Comment <$> field <*> field <*> field

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
  schemaCreated <- tableExists conn "comments"
  unless schemaCreated $
    execute_ conn
      (Query $
       T.concat [ "CREATE TABLE comments ("
                , "id INTEGER PRIMARY KEY, "
                , "user_id INTEGER NOT NULL, "
                , "saved_on TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, "
                , "comment TEXT)"])

-- | Retrieve a user's list of comments
listComments :: Connection -> User -> IO [Comment]
listComments conn (User uid _) =
  query conn "SELECT id,saved_on,comment FROM comments WHERE user_id = ?" (Only uid)

-- | Save a new comment for a user
saveComment :: Connection -> User -> T.Text -> IO ()
saveComment conn (User uid _) c =
  execute conn "INSERT INTO comments (user_id,comment) VALUES (?,?)" (uid, c)
