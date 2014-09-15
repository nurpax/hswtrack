{-# LANGUAGE OverloadedStrings #-}

module Site.Util (
    reader
  , runHttpErrorEitherT
  , hoistHttpError
  , badReq
  , forbiddenReq
  , parseDouble
  , parseInt64
  , tryGetParam
  , getDoubleParam
  , getIntParam
  , getInt64Param
  , getTextParam
  , maybeGetTextParam
  , getJSON
  , writeJSON
  , withDb
  , module X
  , HttpError(..)
  ) where

------------------------------------------------------------------------------
import           Control.Error.Safe (tryJust)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Either
import qualified Data.Aeson as A
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

data HttpError = HttpError Int String

type H = Handler App App


-------------------------------------------------------------------------------
-- | Mark response as 'application/json'
jsonResponse :: MonadSnap m => m ()
jsonResponse = modifyResponse $ setHeader "Content-Type" "application/json"

-------------------------------------------------------------------------------
-- | Set MIME to 'application/json' and write given object into
-- 'Response' body.
writeJSON :: (MonadSnap m, A.ToJSON a) => a -> m ()
writeJSON a = do
  jsonResponse
  writeLBS . A.encode $ a

-------------------------------------------------------------------------------
-- | Try to parse request body as JSON with a default max size of
-- 50000.
getJSON :: (MonadSnap m, A.FromJSON a) => m (Either String a)
getJSON = getBoundedJSON 50000


-------------------------------------------------------------------------------
-- | Parse request body into JSON or return an error string.
getBoundedJSON
    :: (MonadSnap m, A.FromJSON a)
    => Int64
    -- ^ Maximum size in bytes
    -> m (Either String a)
getBoundedJSON n = do
  bodyVal <- A.decode `fmap` readRequestBody n
  return $ case bodyVal of
    Nothing -> Left "Can't find JSON data in POST body"
    Just v -> case A.fromJSON v of
                A.Error e -> Left e
                A.Success a -> Right a


-- | Discard anything after this and return given status code to HTTP
-- client immediately.
finishEarly :: MonadSnap m => Int -> ByteString -> m b
finishEarly code str = do
  modifyResponse $ setResponseStatus code str
  modifyResponse $ addHeader "Content-Type" "text/plain"
  writeBS str
  getResponse >>= finishWith

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

runHttpErrorEitherT :: EitherT HttpError H (H ()) -> H ()
runHttpErrorEitherT e = runEitherT e >>= either err id
  where
    err (HttpError errCode msg) = do
      let m = T.encodeUtf8 . T.pack $ msg
      logError m
      finishEarly errCode m

badReq :: String -> HttpError
badReq msg = HttpError 400 msg

forbiddenReq :: String -> HttpError
forbiddenReq msg = HttpError 403 msg

hoistHttpError :: Monad m => Either String a -> EitherT HttpError m a
hoistHttpError (Left m)  = hoistEither . Left . badReq $ m
hoistHttpError (Right v) = hoistEither . Right $ v

parseDouble :: T.Text -> EitherT HttpError H Double
parseDouble t =
  hoistHttpError (reader T.rational t)

parseInt64 :: T.Text -> EitherT HttpError H Int64
parseInt64 t = hoistHttpError (reader T.decimal t)

tryGetParam :: MonadSnap m => ByteString -> EitherT HttpError m ByteString
tryGetParam p =
  lift (getParam p) >>= tryJust (badReq $ "missing get param '"++ show p ++"'")

getIntParam :: ByteString -> EitherT HttpError H Int
getIntParam n =
  tryGetParam n >>= \p -> hoistHttpError (reader T.decimal . T.decodeUtf8 $ p)

getInt64Param :: ByteString -> EitherT HttpError H Int64
getInt64Param n =
  getTextParam n >>= \p -> parseInt64 p

getDoubleParam :: ByteString -> EitherT HttpError H Double
getDoubleParam n =
  getTextParam n >>= \p -> parseDouble p

getTextParam :: ByteString -> EitherT HttpError H T.Text
getTextParam n =
  tryGetParam n >>= \p -> return . T.decodeUtf8 $ p

maybeGetTextParam :: ByteString -> EitherT HttpError H (Maybe T.Text)
maybeGetTextParam n = do
  p <- lift $ getParam n
  return $ fmap T.decodeUtf8 p
