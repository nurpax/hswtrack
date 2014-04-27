{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

import           Control.Lens
import           Data.Monoid
import           Data.Time (UTCTime)
import qualified Data.Text as T
import           Network.Wreq
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit

testCreateUser :: Assertion
testCreateUser = do
  let login  = "test" :: T.Text
      passwd = "testpass" :: T.Text
  r <- post "http://localhost:8000/rest/new_user" ["login" := login, "password" := passwd]
  1 @=? (1 :: Int)

test1 :: Assertion
test1 = do
  let login  = "test" :: T.Text
      passwd = "testpass" :: T.Text
  r <- post "http://localhost:8000/rest/login" ["login" := login, "password" := passwd]
  let opts = defaults & cookies .~ (r ^. responseCookieJar)
  r <- getWith opts "http://localhost:8000/rest/app"
  print (r ^. responseBody)
  1 @=? (1 :: Int)

main :: IO ()
main = defaultMainWithOpts
       [ testCase "createUser" testCreateUser
       , testCase "login"      test1
       ]
       mempty
