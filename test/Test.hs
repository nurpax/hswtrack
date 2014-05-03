{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables #-}

import qualified Control.Exception as E
import           Control.Lens
import           Data.Aeson.Lens
import           Data.Monoid
import qualified Data.Text as T
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
      | otherwise              = assertBool "unexpected status code" False
    check (Left _)  = assertBool "unexpected exception caught" False
    check (Right r) = assertBool "req should've failed" False

main :: IO ()
main =
  defaultMainWithOpts
  [ testGroup "require auth fail" requireAuthFail
  , buildTest $ createUserTests [("logged in?", testLoggedInOk)]
  , buildTest $ loginUserTests  [("logged in?", testLoggedInOk)]
  ]
  mempty
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

