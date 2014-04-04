{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Site.Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.SqliteSimple

------------------------------------------------------------------------------
data App = App
    { _sess :: Snaplet SessionManager
    , _db   :: Snaplet Sqlite
    , _auth :: Snaplet (AuthManager App)
      -- | If set, testUserOverride will force the application to
      -- bypass login & authentication and instead use a hardcoded
      -- 'test' user for all model accesses.
    , _testUserOverride :: Bool
    }

makeLenses ''App

------------------------------------------------------------------------------
type AppHandler = Handler App App


