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
    }

makeLenses ''App

------------------------------------------------------------------------------
type AppHandler = Handler App App


