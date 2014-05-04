{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables #-}

import qualified Control.Exception as E
import           Control.Lens
import           Data.Aeson.Lens
import qualified Data.ByteString as BS
import           Data.List
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Vector as V
import           Network.Wreq
import qualified Network.HTTP.Client as HT
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (TestList, Test)

mkUrl :: String -> String
mkUrl s = "http://localhost:8000" ++ s

-- Test user login name
login :: T.Text
login = "test"

-- Test user password
passwd :: T.Text
passwd = "testpass"

emptyPostParams :: [(BS.ByteString, BS.ByteString)]
emptyPostParams = []

-- | Create a user and run a list of subtests with cookies acquired
-- from the login process.
createUserTests :: [(String, Options -> Assertion)] -> IO Test
createUserTests tests = do
  r <- post (mkUrl "/rest/new_user") ["login" := login, "password" := passwd]
  let opts = defaults & cookies .~ (r ^. responseCookieJar)
  return
    . testGroup "Tests with created user"
    . map (\(name, test) -> testCase name (test opts)) $ tests

-- | Login a user and run a list of subtests with cookies acquired
-- from the login process.
loginUserTests :: [(String, Options -> Assertion)] -> IO Test
loginUserTests tests = do
  r <- post (mkUrl "/rest/login") ["login" := login, "password" := passwd]
  let opts = defaults & cookies .~ (r ^. responseCookieJar)
  return
    . testGroup "Tests with logged in user"
    . map (\(name, test) -> testCase name (test opts)) $ tests

testLoggedInOk :: Options -> Assertion
testLoggedInOk opts = do
  r <- getWith opts (mkUrl "/rest/app")
  Just True @=? (r ^? responseBody . key "loggedIn" . _Bool)

-- GET requests against 'url' and expect to get error 403 back
testLoggedInFail :: String -> Options -> Assertion
testLoggedInFail url opts = do
  E.try (getWith opts url) >>= check
  where
    check (Left (HT.StatusCodeException s _ _))
      | s ^. statusCode == 403 = assertBool "error ok" True
      | otherwise              = assertFailure "unexpected status code"
    check (Left _)  = assertFailure "unexpected exception caught"
    check (Right _) = assertFailure "req should've failed"

testAddExercise :: T.Text -> T.Text -> Options -> Assertion
testAddExercise name ty opts = do
  r <- postWith opts (mkUrl "/rest/exercise") ["name" := name, "type" := ty]
  -- Verify that the newly created object matches creation params
  name @=? r ^. responseBody . key "name" . _String
  ty   @=? r ^. responseBody . key "type" . _String
  -- This is test code, so it's OK if we fail the below non-exhaustive
  -- pattern match
  let (Just oid) = r ^? responseBody . key "id" . _Integer
  -- Verify that the object ended up in the global list of exercises
  r <- getWith opts (mkUrl "/rest/exercise")
  assertBool "oid should be in list" (oid `elem` exercises r)
  where
    exercises r = r ^. responseBody ^.. values . key "id" . _Integer

listExercises :: Options -> IO [(Integer, T.Text)]
listExercises opts = do
  r <- getWith opts (mkUrl "/rest/exercise")
  return $ r ^. responseBody ^.. values . to nameId
  where
    nameId v = (fromJust $ v ^? key "id" . _Integer, v ^. key "name" . _String)

-- Pure but we return in IO in order to assert missing exercises.
exerciseIdByName :: T.Text -> [(Integer, T.Text)] -> IO Integer
exerciseIdByName name xs =
  case find (\(_,n) -> n == name) xs of
    Nothing -> assertFailure "missing exercise" >> return 0
    Just v  -> return . fst $ v

addReps :: Options -> Integer -> Integer -> Integer -> Integer -> Assertion
addReps opts workoutId exerciseId reps weight = do
  r <- postWith opts (mkUrl "/rest/workout/exercise")
    [ "workoutId"  := workoutId
    , "exerciseId" := exerciseId
    , "reps"       := reps
    , "weight"     := weight
    ]
  Just weight @=? r ^? responseBody . key "weight" . _Integer
  Just reps   @=? r ^? responseBody . key "reps"   . _Integer

testWorkout :: Options -> Assertion
testWorkout opts = do
  exTypes <- listExercises opts
  r <- postWith opts (mkUrl "/rest/workout") emptyPostParams
  let workoutId = fromJust $ r ^? responseBody . key "id" . _Integer
      timestamp = r ^. responseBody . key "time" . _String
  assertBool "time exists" (T.length timestamp > 0)
  ex1Id <- exerciseIdByName "chin-ups" exTypes
  ex2Id <- exerciseIdByName "deadlift" exTypes
  addReps opts workoutId ex1Id 10 0
  addReps opts workoutId ex1Id 5 10
  addReps opts workoutId ex2Id 5 100
  r <- getWith (opts & param "id" .~ [T.pack . show $ workoutId]) (mkUrl "/rest/workout")
  let exercises l   = r ^. responseBody ^.. key "exercises" . values . l
  let exerciseNames = exercises (key "name")
  let exerciseSets  = exercises (key "sets" . _Array)
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

main :: IO ()
main =
  defaultMain
  [ testGroup "Require auth fail" requireAuthFail
  , buildTest $ createUserTests [("Logged in?", testLoggedInOk)]
  , buildTest $ loginUserTests  [ ("Logged in?",     testLoggedInOk)
                                , ("Add exercise 1", testAddExercise "chin-ups" "BW")
                                , ("Add exercise 2", testAddExercise "deadlift" "W")
                                , ("Create workout", testWorkout)
                                ]
  ]
  where
    requireAuthFail =
      map (\u -> testCase u (testLoggedInFail (mkUrl u) defaults)) authReqd
    -- REST entry points which require user to be logged in
    authReqd = [ "/rest/app"
               , "/rest/weights"
               , "/rest/notes"
               , "/rest/exercise"
               , "/rest/workout/exercise"
               , "/rest/workout"
               , "/rest/workout"
               , "/rest/stats/workout"
               ]

