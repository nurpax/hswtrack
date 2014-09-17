{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, Rank2Types #-}

module Weight where

import           Control.Lens
import           Control.Monad (void)
import           Data.Aeson (Value)
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Network.Wreq
import           Test.HUnit hiding (TestList, Test)

import           Test

respPayload :: Traversal' (Response LBS.ByteString) Value
respPayload = responseBody . key "payload"

testAddNote :: Options -> Assertion
testAddNote opts = do
  let date      = "2014-04-04" :: T.Text
      wrongDate = "2014-04-05" :: T.Text
  r <- getWith opts (mkUrl "/rest/note")
  [] @=? r ^.. respPayload . values
  void $ postWith opts (mkUrl "/rest/note") [ "date" := date
                                            , "text" := ("note text"  :: T.Text)
                                            ]
  r <- getWith (opts & param "date" .~ [date]) (mkUrl "/rest/note")
  1 @=? (length $ r ^.. respPayload . values)
  let Just noteText = r ^? respPayload . nth 0 . key "text" . _String
      Just noteId   = r ^? respPayload . nth 0 . key "id" . _Integer
  "note text" @=? noteText
  r <- getWith (opts & param "date" .~ [wrongDate]) (mkUrl "/rest/note")
  0 @=? (length $ r ^.. respPayload . values)
  void $ deleteWith (opts & setParam "id" noteId) (mkUrl "/rest/note")
  r <- getWith (opts & param "date" .~ [date]) (mkUrl "/rest/note")
  0 @=? (length $ r ^.. respPayload . values)

testSetWeight :: Options -> Assertion
testSetWeight opts = do
  let date = "2014-04-04" :: T.Text
  r <- postWith opts
         (mkUrl "/rest/weight")
         ["date"   := date, "weight" := (80 :: Integer)]
  let Just wId =  r ^? respPayload . key "id"     . _Integer
  Just 80     @=? r ^? respPayload . key "weight" . _Double
  r <- getWeights date Nothing
  1 @=? (length $ r ^.. respPayload . values)
  Just 80  @=? r ^? respPayload . nth 0 . key "weight" . _Double
  Just wId @=? r ^? respPayload . nth 0 . key "id"     . _Integer
  r <- getWith (opts & param "date" .~ [date]) (mkUrl "/rest/app")
  Just 80  @=? r ^? respPayload . key "context" . key "weight" . key "weight" . _Double
  void $ deleteWith (opts & setParam "id" wId) (mkUrl "/rest/weight")
  r <- getWeights date (Just 1)
  0 @=? (length $ r ^.. respPayload . values)
  where
    getWeights date (nDays :: Maybe Integer) =
      getWith (opts & param    "date" .~ [date]
                    & setParam "days" (fromMaybe 0 nDays))
              (mkUrl "/rest/weight")
