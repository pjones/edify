-- |
--
-- Copyright:
--   This file is part of the package Edify. It is subject to the
--   license terms in the LICENSE file found in the top-level
--   directory of this distribution and at:
--
--     https://github.com/pjones/edify
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the terms
--   contained in the LICENSE file.
--
-- License: Apache-2.0
module Edify.Compiler.User
  ( UserF (..),
    User,
    defaultUserConfigFile,
    readUserConfig,
    resolve,
  )
where

import qualified Edify.Project.Config as Project
import qualified Edify.System.Input as Input
import Edify.Text.JSON
import qualified System.Directory as Directory
import System.FilePath ((</>))

-- | User configuration.
--
-- @since 0.5.0.0
data UserF (f :: Readiness) = User
  { -- | Directory where command fingerprints are stored.
    userCommandAllowDir :: Default f FilePath,
    -- | Default project configuration.
    userProjectConfig :: Project.ConfigF Parsed
  }
  deriving stock (Generic)

-- | Resolved user configuration.
--
-- @since 0.5.0.0
type User = UserF Resolved

deriving via (GenericJSON (UserF Parsed)) instance ToJSON (UserF Parsed)

deriving via (GenericJSON (UserF Parsed)) instance FromJSON (UserF Parsed)

instance Semigroup (UserF Parsed) where
  (<>) x y =
    User
      { userCommandAllowDir = userCommandAllowDir x <|> userCommandAllowDir y,
        userProjectConfig = userProjectConfig x <> userProjectConfig y
      }

instance Monoid (UserF Parsed) where
  mempty = User mempty mempty

-- | Default file location for the user configuration.
--
-- @since 0.5.0.0
defaultUserConfigFile :: MonadIO m => m FilePath
defaultUserConfigFile =
  liftIO (Directory.getXdgDirectory Directory.XdgConfig "edify")
    <&> (</> "config.yml")

-- | Read the user configuration file.
--
-- @since 0.5.0.0
readUserConfig :: MonadIO m => FilePath -> m (Either Input.Error (UserF Parsed))
readUserConfig = Input.decodeFromFile Input.ReadWithoutFingerprint

-- | Resolve a 'UserF Parsed' value to a final 'UserF Identity' value.
--
-- @since 0.5.0.0
resolve :: forall m. MonadIO m => UserF Parsed -> m (UserF Resolved)
resolve User {..} =
  User
    <$> maybe defaultAllowDir pure userCommandAllowDir
    <*> pure userProjectConfig
  where
    defaultAllowDir :: m FilePath
    defaultAllowDir =
      Directory.getXdgDirectory Directory.XdgData ("edify" </> "allow")
        & liftIO
