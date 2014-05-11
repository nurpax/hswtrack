{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, Rank2Types #-}

module Test where

import qualified Control.Exception as E
import           Control.Lens
import           Data.Aeson.Lens
import qualified Data.ByteString as BS
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

emptyPostParams :: [(BS.ByteString, BS.ByteString)]
emptyPostParams = []

setParam :: Show a => T.Text -> a -> Options -> Options
setParam name v = param name .~ [T.pack . show $ v]

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
    check (Left (HT.StatusCodeException s _ _)) =
      assertBool "error ok" (s ^. statusCode == 403)
    check (Left _)  = assertFailure "unexpected exception caught"
    check (Right _) = assertFailure "req should've failed"


