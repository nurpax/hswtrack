{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, Rank2Types #-}

import           Network.Wreq
import           Test.Framework
import           Test.Framework.Providers.HUnit

import           Test
import           Workout
import           Weight

main :: IO ()
main =
  defaultMain
  [ testGroup "Require auth fail" requireAuthFail
  , buildTest $ createUserTests [("Logged in?", testLoggedInOk)]
  , buildTest $ loginUserTests  [ ("Logged in?",       testLoggedInOk)
                                , ("Add exercise 1",   testAddExercise "chin-ups" "BW")
                                , ("Add exercise 2",   testAddExercise "deadlift" "W")
                                , ("Create workout",   testWorkout)
                                , ("Delete sets",      testSetDelete)
                                , ("Delete sets 2",    testSetDelete2)
                                , ("Add note",         testAddNote)
                                , ("Set/clear weight", testSetWeight)
                                ]
  , testCase "workout perms" testAccessRights
  ]
  where
    requireAuthFail =
      map (\u -> testCase u (testLoggedInFail (mkUrl u) defaults)) authReqd
    -- REST entry points which require user to be logged in
    authReqd = [ "/rest/app"
               , "/rest/weight"
               , "/rest/note"
               , "/rest/workout/exercise"
               , "/rest/workout"
               , "/rest/workout"
               , "/rest/stats/workout"
               ]
