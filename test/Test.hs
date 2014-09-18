{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, Rank2Types #-}

module Test where

import qualified Control.Exception as E
import           Control.Lens hiding ((.=))
import           Data.Aeson (object, (.=))
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
login, login2 :: T.Text
login  = "test"
login2 = "test2"

-- Test user password
passwd, passwd2 :: T.Text
passwd  = "testpass"
passwd2 = "testpass2"

emptyPostParams :: [(BS.ByteString, BS.ByteString)]
emptyPostParams = []

setParam :: Show a => T.Text -> a -> Options -> Options
setParam name v = param name .~ [T.pack . show $ v]

-- | Create two tests users and run a list of subtests with cookies
-- acquired from the login process of user 'login'.
createUserTests :: [(String, Options -> Assertion)] -> IO Test
createUserTests tests = do
  r <- post (mkUrl "/rest/new_user") ["login" := login,  "password" := passwd]
  _ <- post (mkUrl "/rest/new_user") ["login" := login2, "password" := passwd2]
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

-- GET requests against 'url' and expect to get error back
getExpectHttpError :: Options -> String -> Int -> Assertion
getExpectHttpError opts url errCode = do
  E.try (getWith opts url) >>= check
  where
    check (Left (HT.StatusCodeException s _ _)) =
      assertBool "error ok" (s ^. statusCode == errCode)
    check (Left _)  = assertFailure "unexpected exception caught"
    check (Right _) = assertFailure "req should've failed"

-- GET requests against 'url' and expect to get error 403 back
testLoggedInFail :: String -> Options -> Assertion
testLoggedInFail url opts = getExpectHttpError opts url 403

testChangePassword :: Assertion
testChangePassword = do
  let newPasswd = "new_pass" :: T.Text
  -- Login
  r <- post (mkUrl "/rest/login") ["login" := login, "password" := passwd]
  let opts1 = defaults & cookies .~ (r ^. responseCookieJar)
  testLoggedInOk opts1
  let changePass = object [ "password" .= newPasswd ]
  r <- putWith opts1 (mkUrl "/rest/user") changePass
  let opts2 = defaults & cookies .~ (r ^. responseCookieJar)
  testLoggedInOk opts2
  r <- post (mkUrl "/rest/login") ["login" := login, "password" := passwd]
  let opts3 = defaults & cookies .~ (r ^. responseCookieJar)
  testLoggedInFail (mkUrl "/rest/app") opts3
  r <- post (mkUrl "/rest/login") ["login" := login, "password" := newPasswd]
  let opts4 = defaults & cookies .~ (r ^. responseCookieJar)
  testLoggedInOk opts4
  -- Change the passwd back to original, so that the next run with the
  -- same database won't start failing tests.
  r <- putWith opts4 (mkUrl "/rest/user") $ object [ "password" .= passwd ]
  return ()
