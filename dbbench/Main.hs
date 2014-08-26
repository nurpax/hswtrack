{-# LANGUAGE OverloadedStrings, BangPatterns, ScopedTypeVariables #-}

import           Control.Applicative
import           Control.Exception (bracket)
import           Criterion.Config (defaultConfig)
import           Criterion.Main
import qualified Data.Text as T

import qualified Database.SQLite.Simple as SQL
import qualified Model as M

benchmarkQueryWorkout :: SQL.Connection -> IO ()
benchmarkQueryWorkout conn = do
  vals <- M.queryWorkout conn (Just (M.User 2 "j")) 138
  return ()

benchmarkQueryPastWorkouts :: SQL.Connection -> IO ()
benchmarkQueryPastWorkouts conn = do
  vals <- M.queryPastWorkouts conn (M.User 2 "j") (read "2014-08-26 00:00:00") 14
  return ()

main :: IO ()
main = do
  bracket (SQL.open "hswtrack.db") SQL.close go
  where
    go conn = do
--      SQL.setTrace conn (Just (putStrLn . T.unpack))
      defaultMainWith defaultConfig (return ())
        [ bench "queryWorkout"       $ benchmarkQueryWorkout conn
        , bench "queryPastWorkouts"  $ benchmarkQueryPastWorkouts conn]
