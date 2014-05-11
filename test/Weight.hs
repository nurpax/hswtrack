{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, Rank2Types #-}

module Weight where

import           Control.Lens
import           Control.Monad (void)
import           Data.Aeson.Lens
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Network.Wreq
import           Test.HUnit hiding (TestList, Test)

import           Test

testAddNote :: Options -> Assertion
testAddNote opts = do
  let date      = "2014-04-04" :: T.Text
      wrongDate = "2014-04-05" :: T.Text
  r <- getWith opts (mkUrl "/rest/notes")
  [] @=? r ^.. responseBody . values
  void $ postWith opts (mkUrl "/rest/note") [ "date" := date
                                            , "text" := ("note text"  :: T.Text)
                                            ]
  r <- getWith (opts & param "date" .~ [date]) (mkUrl "/rest/notes")
  1 @=? (length $ r ^.. responseBody . values)
  let Just noteText = r ^? responseBody . nth 0 . key "text" . _String
      Just noteId   = r ^? responseBody . nth 0 . key "id" . _Integer
  "note text" @=? noteText
  r <- getWith (opts & param "date" .~ [wrongDate]) (mkUrl "/rest/notes")
  0 @=? (length $ r ^.. responseBody . values)
  void $ deleteWith (opts & setParam "id" noteId) (mkUrl "/rest/note")
  r <- getWith (opts & param "date" .~ [date]) (mkUrl "/rest/notes")
  0 @=? (length $ r ^.. responseBody . values)

testSetWeight :: Options -> Assertion
testSetWeight opts = do
  let date = "2014-04-04" :: T.Text
  r <- postWith opts
         (mkUrl "/rest/weight")
         ["date"   := date, "weight" := (80 :: Integer)]
  let Just wId =  r ^? responseBody . key "id"     . _Integer
  Just 80     @=? r ^? responseBody . key "weight" . _Double
  r <- getWeights date Nothing
  1 @=? (length $ r ^.. responseBody . values)
  Just 80  @=? r ^? responseBody . nth 0 . key "weight" . _Double
  Just wId @=? r ^? responseBody . nth 0 . key "id"     . _Integer
  void $ deleteWith (opts & setParam "id" wId) (mkUrl "/rest/weight")
  r <- getWeights date (Just 1)
  0 @=? (length $ r ^.. responseBody . values)
  where
    getWeights date (nDays :: Maybe Integer) =
      getWith (opts & param    "date" .~ [date]
                    & setParam "days" (fromMaybe 0 nDays))
              (mkUrl "/rest/weights")
