{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

import           Control.Lens
import           Data.Monoid
import           Data.Time (UTCTime)
import qualified Data.Text as T
import           Network.Wreq
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit hiding (TestList, Test)

-- | Create a user and run a list of subtests with cookies acquired
-- from the login process.
createUserTests :: [(String, Options -> Assertion)] -> IO Test
createUserTests tests = do
  let login  = "test" :: T.Text
      passwd = "testpass" :: T.Text
  r <- post "http://localhost:8000/rest/new_user" ["login" := login, "password" := passwd]
  let opts = defaults & cookies .~ (r ^. responseCookieJar)
  return
    . testGroup "Tests with created user"
    . map (\(name, test) -> testCase name (test opts)) $ tests

-- | Login a user and run a list of subtests with cookies acquired
-- from the login process.
loginUserTests :: [(String, Options -> Assertion)] -> IO Test
loginUserTests tests = do
  let login  = "test" :: T.Text
      passwd = "testpass" :: T.Text
  r <- post "http://localhost:8000/rest/login" ["login" := login, "password" := passwd]
  let opts = defaults & cookies .~ (r ^. responseCookieJar)
  return
    . testGroup "Tests with logged in user"
    . map (\(name, test) -> testCase name (test opts)) $ tests

testLoggedInOk :: Options -> Assertion
testLoggedInOk opts = do
  r <- getWith opts "http://localhost:8000/rest/app"
  print (r ^. responseBody)
  1 @=? (1 :: Int)

main :: IO ()
main = defaultMainWithOpts
       [ buildTest $ createUserTests [("logged in?", testLoggedInOk)]
       , buildTest $ loginUserTests  [("logged in?", testLoggedInOk)]
       ]
       mempty
