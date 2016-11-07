{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

{-|

Main module.

-}

module Main

where

import Data.Functor
import Data.Monoid

import Data.Aeson.Encode.Pretty
import qualified Data.Aeson.Types as A

import qualified Data.Text as T (pack)
import qualified Data.ByteString.Lazy as LB (putStr)
import qualified Data.ByteString.UTF8 as BU (fromString)
import qualified Data.HashMap.Lazy as M

import System.Console.CmdArgs.Implicit

import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile


-- | Rank-2 type for action applicable to AuthManager and AuthUser.
type AuthUserAction = IAuthBackend r => r -> AuthUser -> IO ()


-- | Action for --read mode: show pretty JSON auth user entry.
readAction :: AuthUserAction
readAction _ au = LB.putStr $ encodePretty au


-- | Containers for which null predicate is defined.
class PossiblyNull a where
    null :: a

instance PossiblyNull [a] where
    null = []

instance PossiblyNull (Maybe a) where
    null = Nothing

instance PossiblyNull (M.HashMap k v) where
    null = M.empty

-- | Monoid under choosing non-null container.
--
-- We can map empty containers to Nothing and use standard monoid for
-- Nothing as well.
newtype NullMonoid a = NullMonoid { getContainer :: a }

instance (Eq a, PossiblyNull a) => Monoid (NullMonoid a) where
    mempty = NullMonoid Main.null
    (NullMonoid x) `mappend` (NullMonoid y) =
        NullMonoid (if x == Main.null then y else x)


-- | Make 'AuthUserAction' which will replace user with the supplied
-- one. In case new user record does not set new password, roles or
-- meta, preserve old values.
makeUpdateAction :: AuthUser -> AuthUserAction
makeUpdateAction newUser =
    \amgr oldUser ->
        let
            pick how = getContainer $ mconcat $
                       map (NullMonoid . how) [newUser, oldUser]
        in
          save amgr newUser{ userId = userId oldUser
                           , userPassword = pick userPassword
                           , userRoles = pick userRoles
                           , userMeta = pick userMeta
                           }
           >> return ()


-- | Create new AuthUser object from data supplied from command line.
--
-- Has to be monadic due to 'setPassword' usage (hashing requires IO)
-- (in case of non-'Nothing' password).
buildAuthUser :: String             -- ^ User
              -> Maybe String       -- ^ Password
              -> [String]           -- ^ Roles
              -> [(String, String)] -- ^ List of key-value pairs of
                                    -- user metadata
              -> IO AuthUser
buildAuthUser l p rs mt =
    let
        login = T.pack l
        pass = BU.fromString <$> p
        roles = map (Role . BU.fromString) $ rs
        meta = M.fromList $
               map (\(k, v) -> (T.pack k, A.String $ T.pack v)) mt
        au' = defAuthUser{ userLogin = login
                         , userRoles = roles
                         , userMeta = meta
                         }
    in
      do
        case pass of
          Nothing -> return au'
          Just pw -> setPassword au' pw


-- | Get user from backend by login and apply 'AuthUserAction' to it.
mgrOldUser :: IAuthBackend r => r
           -> String
           -> AuthUserAction
           -> IO ()
mgrOldUser amgr l f =
    let
        login = T.pack l
    in
      do
        au <- lookupByLogin amgr login
        case au of
          Just found -> f amgr found
          Nothing -> ioError $ userError $
                     l ++ ": user not found"


-- | Possible modes of operation.
data OpMode = Create | Read | Modify | Delete
              deriving (Show, Data, Typeable)


-- | Default instance for CmdArg.
instance Default OpMode where
    def = Read


-- | Holds all options passed from command-line.
data Options = Options
    { mode     :: OpMode
    , user     :: Maybe String
    , password :: Maybe String
    , json     :: String
    , role     :: [String]
    , key      :: [String]
    , value    :: [String]
    }
    deriving (Show, Data, Typeable)


modifyHelp :: String
modifyHelp = "Modify password or replace roles and meta of existing user. " ++
             "If any of -p/-r/-k,-v flags is not set, previous value " ++
             "of that flag is preserved for account"


main :: IO ()
main =
    let
        sample = Options
                 { mode =
                   enum [ Read &= help "Read existing user"
                        , Create &= help "Create new user"
                        , Modify &= help modifyHelp
                        , Delete &= help "Delete user"] &= groupname "Mode"
                 , user = def &= help "User login" &= groupname "Other flags"
                 , password = def
                 , role = def &= name "o"
                   &= help "User role. May be specified multiple times"
                 , key = def &= name "k"
                   &= help "User meta key. Must be followed by value option"
                 , value = def &= name "v"
                   &= help "User meta value."
                 , json = "users.json"
                   &= typFile
                   &= help "Path to JsonFile database"
                 }
                 &= program "snap-auth-cli"
    in do
      -- RecordWildCards
      Options{..} <- cmdArgs $ sample
      amgr <- mkJsonAuthMgr json
      case (mode, user, password) of
        (_, Nothing, _) -> ioError $ userError "No user selected"
        (Read, Just l, _) -> mgrOldUser amgr l readAction
        (Delete, Just l, _) -> mgrOldUser amgr l destroy
        -- Require password for new users
        (Create, _, Nothing) -> ioError $ userError "No password set"
        (_, Just l, pw) ->
            do
                au <- buildAuthUser l pw role (zip key value)
                case mode of
                  Modify -> mgrOldUser amgr l (makeUpdateAction au) >> return ()
                  Create -> save amgr au >> return ()