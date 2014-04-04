{-# LANGUAGE OverloadedStrings #-}

module Site.Util (
    reader
  , logFail
  , logRunEitherT
  , parseDouble
  , parseInt64
  , tryGetParam
  , getDoubleParam
  , getIntParam
  , getInt64Param
  , getTextParam
  , maybeGetTextParam
  , withDb
  , writeJSON
  , module X
  ) where

------------------------------------------------------------------------------
import           Control.Error.Safe (tryJust)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Either
import           Data.Aeson (ToJSON, encode)
import           Data.ByteString (ByteString)
import           Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import           Database.SQLite.Simple as S
import           Snap.Core as X
import           Snap.Snaplet as X
import           Snap.Snaplet.Auth as X
import           Snap.Snaplet.SqliteSimple
------------------------------------------------------------------------------
import           Site.Application as X
------------------------------------------------------------------------------

type H = Handler App App

-- | Discard anything after this and return given status code to HTTP
-- client immediately.
finishEarly :: MonadSnap m => Int -> ByteString -> m b
finishEarly code str = do
  modifyResponse $ setResponseStatus code str
  modifyResponse $ addHeader "Content-Type" "text/plain"
  writeBS str
  getResponse >>= finishWith

-- | Finish early with error code 400
badReq :: MonadSnap m => ByteString -> m b
badReq = finishEarly 400

-- | Mark response as 'application/json'
jsonResponse :: MonadSnap m => m ()
jsonResponse = modifyResponse $ setHeader "Content-Type" "application/json"

-- | Set MIME to 'application/json' and write given object into
-- 'Response' body.
writeJSON :: (MonadSnap m, ToJSON a) => a -> m ()
writeJSON a = do
  jsonResponse
  writeLBS . encode $ a

-- | Run an IO action with an SQLite connection
withDb :: (S.Connection -> IO a) -> H a
withDb action =
  withTop db . withSqlite $ \conn -> action conn

reader :: T.Reader a -> T.Text -> Either String a
reader p s =
  case p s of
    Right (a, "") -> return a
    Right (_, _) -> Left "readParser: input not exhausted"
    Left e -> Left e

-- | Log Either Left values or run the Handler action.  To be used in
-- situations where to user shouldn't see an error (either due to it
-- being irrelevant or due to security) but we want to leave a trace
-- of the error and finish with a HTTP error code 400.
logFail :: Either String (H ()) -> H ()
logFail = either failReq id
  where
    failReq msg = do
      let e = T.encodeUtf8 . T.pack $ msg
      logError e
      badReq e


logRunEitherT :: EitherT String H (H ()) -> H ()
logRunEitherT e = runEitherT e >>= logFail

parseDouble :: T.Text -> EitherT String H Double
parseDouble t =
  hoistEither (reader T.rational t)

parseInt64 :: T.Text -> EitherT String H Int64
parseInt64 t = hoistEither (reader T.decimal t)

tryGetParam :: MonadSnap m => ByteString -> EitherT String m ByteString
tryGetParam p =
  lift (getParam p) >>= tryJust ("missing get param '"++ show p ++"'")

getIntParam :: ByteString -> EitherT String H Int
getIntParam n =
  tryGetParam n >>= \p -> hoistEither (reader T.decimal . T.decodeUtf8 $ p)

getInt64Param :: ByteString -> EitherT String H Int64
getInt64Param n =
  getTextParam n >>= \p -> parseInt64 p

getDoubleParam :: ByteString -> EitherT String H Double
getDoubleParam n =
  getTextParam n >>= \p -> parseDouble p

getTextParam :: ByteString -> EitherT String H T.Text
getTextParam n =
  tryGetParam n >>= \p -> return . T.decodeUtf8 $ p

maybeGetTextParam :: ByteString -> EitherT String H (Maybe T.Text)
maybeGetTextParam n = do
  p <- lift $ getParam n
  return $ fmap T.decodeUtf8 p
