{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, Rank2Types #-}

module Workout where

import           Control.Lens hiding ((.=))
import           Control.Monad (void)
import           Data.Aeson (object, Value, (.=))
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as LBS
import           Data.List
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Network.Wreq
import           Test.HUnit hiding (TestList, Test)

import           Test

type Exercise = (Integer, T.Text)

respPayload :: Traversal' (Response LBS.ByteString) Value
respPayload = responseBody . key "payload"

testAddExercise :: T.Text -> T.Text -> Options -> Assertion
testAddExercise name ty opts = do
  r <- postWith opts (mkUrl "/rest/exercise") ["name" := name, "type" := ty]
  -- Verify that the newly created object matches creation params
  "application/json" @=? r ^. responseHeader "Content-Type"
  name @=? r ^. respPayload . key "name" . _String
  ty   @=? r ^. respPayload . key "type" . _String
  -- This is test code, so it's OK if we fail the below non-exhaustive
  -- pattern match
  let (Just oid) = r ^? respPayload . key "id" . _Integer
  -- Verify that the object ended up in the global list of exercises
  r <- getWith opts (mkUrl "/rest/exercise")
  assertBool "oid should be in list" (oid `elem` exercises r)
  where
    exercises r = r ^.. respPayload . values . key "id" . _Integer

listExercises :: Options -> IO [(Integer, T.Text)]
listExercises opts = do
  r <- getWith opts (mkUrl "/rest/exercise")
  return $ r ^.. respPayload . values . to nameId
  where
    nameId v = (fromJust $ v ^? key "id" . _Integer, v ^. key "name" . _String)

-- Pure but we return in IO in order to assert missing exercises.
exerciseIdByName :: T.Text -> [Exercise] -> IO Integer
exerciseIdByName name xs =
  case find (\(_,n) -> n == name) xs of
    Nothing -> assertFailure "missing exercise" >> return 0
    Just v  -> return . fst $ v

addReps :: Options -> Integer -> Integer -> Integer -> Double -> Assertion
addReps opts workoutId exerciseId reps weight = do
  r <- postWith opts (mkUrl "/rest/workout/exercise")
    [ "workoutId"  := workoutId
    , "exerciseId" := exerciseId
    , "reps"       := reps
    , "weight"     := weight
    ]
  Just weight @=? r ^? respPayload . key "weight" . _Double
  Just reps   @=? r ^? respPayload . key "reps"   . _Integer

deleteSet :: Options -> Integer -> Assertion
deleteSet opts setId = do
  let opts' = opts & setParam "id" setId
  void $ deleteWith opts' (mkUrl "/rest/workout/exercise")
  return ()

exercises :: Traversal' (Response LBS.ByteString) Value
exercises = respPayload . key "exercises" . values

addWorkout :: Options -> [Exercise] -> [(T.Text, [(Integer, Double)])] -> IO Integer
addWorkout opts exTypes sets = do
  r <- postWith opts (mkUrl "/rest/workout") emptyPostParams
  let workoutId = fromJust $ r ^? respPayload . key "id" . _Integer
      timestamp = r ^. respPayload . key "time" . _String
  assertBool "time exists" (T.length timestamp > 0)
  mapM_ (addSets workoutId) sets
  return workoutId
  where
    addSets workoutId (name, sets) = do
      exId <- exerciseIdByName name exTypes
      mapM_ (\(reps, weight) -> addReps opts workoutId exId reps weight) sets

testWorkout :: Options -> Assertion
testWorkout opts = do
  exTypes   <- listExercises opts
  workoutId <- addWorkout opts exTypes [ ("chin-ups", [(10,0), (5, 10)])
                                       , ("deadlift", [(5, 100)])
                                       ]
  r <- getWith (opts & setParam "id" workoutId) (mkUrl "/rest/workout")
  let exerciseSets  = r ^.. exercises . key "sets" . _Array
  let exerciseNames = r ^.. exercises . key "name" . _String
  "chin-ups" @=? (exerciseNames !! 0)
  "deadlift" @=? (exerciseNames !! 1)
  let ex1Sets = (exerciseSets !! 0)
      ex2Sets = (exerciseSets !! 1)
  Just 10  @=? (ex1Sets V.! 0) ^? key "reps"   . _Integer
  Just 0   @=? (ex1Sets V.! 0) ^? key "weight" . _Integer
  Just 5   @=? (ex1Sets V.! 1) ^? key "reps"   . _Integer
  Just 10  @=? (ex1Sets V.! 1) ^? key "weight" . _Integer
  Just 5   @=? (ex2Sets V.! 0) ^? key "reps"   . _Integer
  Just 100 @=? (ex2Sets V.! 0) ^? key "weight" . _Integer

testWorkoutTime :: Options -> Assertion
testWorkoutTime opts = do
  exTypes   <- listExercises opts
  workoutId <- addWorkout opts exTypes [ ("planking", [(1, 60)]) ]
  r <- getWith (opts & setParam "id" workoutId) (mkUrl "/rest/workout")
  let exerciseSets  = r ^.. exercises . key "sets" . _Array
  let exerciseNames = r ^.. exercises . key "name" . _String
  "planking" @=? (exerciseNames !! 0)
  let ex1Sets = (exerciseSets !! 0)
  Just 1    @=? (ex1Sets V.! 0) ^? key "reps"   . _Integer
  Just 60.0 @=? (ex1Sets V.! 0) ^? key "weight" . _Double

queryWorkoutSetIds :: Options -> Integer -> IO [Integer]
queryWorkoutSetIds opts workoutId = do
  r <- getWith (opts & setParam "id" workoutId) (mkUrl "/rest/workout")
  return $ r ^.. exercises . key "sets" . values . key "id" . _Integer

testSetDelete :: Options -> Assertion
testSetDelete opts = do
  exTypes   <- listExercises opts
  workoutId <- addWorkout opts exTypes [("chin-ups", [(10,0), (5, 10)])]
  setIds    <- queryWorkoutSetIds opts workoutId
  2 @=? length setIds
  deleteSet opts (setIds !! 0)
  setIds'   <- queryWorkoutSetIds opts workoutId
  1 @=? length setIds'
  setIds !! 1 @=? setIds' !! 0
  deleteSet opts (setIds !! 1)
  setIds''  <- queryWorkoutSetIds opts workoutId
  [] @=? setIds''

testSetDelete2 :: Options -> Assertion
testSetDelete2 opts = do
  exTypes   <- listExercises opts
  workoutId <- addWorkout opts exTypes [ ("chin-ups", [(10, 0)])
                                       , ("deadlift", [(5, 100)])]
  setIds    <- queryWorkoutSetIds opts workoutId
  r <- getWith (opts & setParam "id" workoutId) (mkUrl "/rest/workout")
  Just False @=? r ^? respPayload . key "public" . _Bool
  let exerciseNames = r ^.. exercises . key "name" . _String
  ["chin-ups", "deadlift"]  @=? exerciseNames
  deleteSet opts (setIds !! 0)
  r <- getWith (opts & setParam "id" workoutId) (mkUrl "/rest/workout")
  let exerciseNames = r ^.. exercises . key "name" . _String
  ["deadlift"]  @=? exerciseNames

-- Create a workout and test that it can be accessed using the logged
-- in user.  Then try that it cannot be accessed without being logged
-- in.  Then make it public and ensure it can again be accessed.
testAccessRights :: Assertion
testAccessRights = do
  -- Login
  r <- post (mkUrl "/rest/login") ["login" := login, "password" := passwd]
  let opts1 = defaults & cookies .~ (r ^. responseCookieJar)
  exTypes   <- listExercises opts1
  workoutId <- addWorkout opts1 exTypes [ ("chin-ups", [(10,0), (5, 10)])
                                        , ("deadlift", [(5, 100)])
                                        ]
  -- Logged out access should fail
  getExpectHttpError (defaults & setParam "id" workoutId) (mkUrl "/rest/workout") 403
  -- Logged out access shouldn't succeed
  r <- getWith (opts1 & setParam "id" workoutId) (mkUrl "/rest/workout")
  Just False @=? r ^? respPayload . key "public" . _Bool
  -- Login with the 'test2' user and verify that we cannot read the
  -- workout from user 'test'
  r <- post (mkUrl "/rest/login") ["login" := login2, "password" := passwd2]
  let opts2 = defaults & cookies .~ (r ^. responseCookieJar)
  -- Logged out access should fail
  getExpectHttpError (defaults & setParam "id" workoutId) (mkUrl "/rest/workout") 403
  getExpectHttpError (opts2 & setParam "id" workoutId) (mkUrl "/rest/workout") 403
  -------------------------------------------------------
  -- Now make the post public and try again.  Anon access should
  -- succeed now
  -------------------------------------------------------
  r <- post (mkUrl "/rest/login") ["login" := login, "password" := passwd]
  let opts = defaults & cookies .~ (r ^. responseCookieJar)
      parms = object [ "id"     .= workoutId
                     , "public" .= True
                     ]
  r <- putWith opts (mkUrl "/rest/workout") parms
  Just True @=? r ^? respPayload . key "public" . _Bool
  let Just loggedInUserId = r ^? responseBody . key "userId" . _Integer
  let Just workoutUserId  = r ^? respPayload  . key "userId" . _Integer
  loggedInUserId @=? workoutUserId
  -- User 'test2' should be able to access the public workout now
  r <- post (mkUrl "/rest/login") ["login" := login2, "password" := passwd2]
  let opts2 = defaults & cookies .~ (r ^. responseCookieJar)
  r <- getWith (opts2 & setParam "id" workoutId) (mkUrl "/rest/workout")
  Just True @=? r ^? respPayload . key "public" . _Bool
  let Just loggedInUserId2 = r ^? responseBody . key "userId" . _Integer
  let Just workoutUserId2  = r ^? respPayload  . key "userId" . _Integer
  -- The workout was created by user 'test' but we're logged in as
  -- 'test2'.  So the user id's in the below shouldn't match.
  assertBool "loginId must not match workout owner id" (loggedInUserId2 /= workoutUserId2)
