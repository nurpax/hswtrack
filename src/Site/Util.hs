{-# LANGUAGE OverloadedStrings #-}

module Site.Util (
    reader
  , logFail
  , logRunEitherT
  , parseDouble
  , tryGetParam
  , getDoubleParam
  , getDoubleParamOrEmpty
  , getIntParam
  , getTextParam
  ) where

import           Control.Error.Safe (tryJust)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Either
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

import           Snap.Core
import           Snap.Snaplet
import           Site.Application

type H = Handler App App

reader :: T.Reader a -> T.Text -> Either String a
reader p s =
  case p s of
    Right (a, "") -> return a
    Right (_, _) -> Left "readParser: input not exhausted"
    Left e -> Left e

-- | Log Either Left values or run the Handler action.  To be used in
-- situations where to user shouldn't see an error (either due to it
-- being irrelevant or due to security) but we want to leave a trace
-- of the error case anyway.
logFail :: Either String (H ()) -> H ()
logFail = either (logError . T.encodeUtf8 . T.pack) id


logRunEitherT :: EitherT String H (H ()) -> H ()
logRunEitherT e = runEitherT e >>= logFail

parseDouble :: T.Text -> EitherT String H Double
parseDouble t =
  hoistEither (reader T.rational $ t)

tryGetParam :: MonadSnap m => ByteString -> EitherT [Char] m ByteString
tryGetParam p =
  lift (getParam p) >>= tryJust ("missing get param '"++ show p ++"'")

getIntParam :: ByteString -> EitherT String H Int
getIntParam n =
  tryGetParam n >>= \p -> hoistEither (reader T.decimal . T.decodeUtf8 $ p)

getDoubleParam :: ByteString -> EitherT String H Double
getDoubleParam n =
  getTextParam n >>= \p -> parseDouble p

getDoubleParamOrEmpty :: ByteString -> EitherT String H (Maybe Double)
getDoubleParamOrEmpty n = do
  v <- getTextParam n
  if v == "" then return Nothing else parseDouble v >>= return . Just

getTextParam :: ByteString -> EitherT String H T.Text
getTextParam n =
  tryGetParam n >>= \p -> return . T.decodeUtf8 $ p
