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
    resolve,
  )
where

import qualified Edify.Compiler.Project as Project
import Edify.JSON
import qualified System.Directory as Directory
import System.FilePath ((</>))

-- | User configuration.
--
-- @since 0.5.0.0
data UserF (f :: Type -> Type) = User
  { -- | Directory where command fingerprints are stored.
    userCommandAllowDir :: Default f FilePath,
    -- | Default project configuration.
    userProjectConfig :: Project.ProjectF Maybe
  }
  deriving stock (Generic)

deriving via (GenericJSON (UserF Maybe)) instance ToJSON (UserF Maybe)

deriving via (GenericJSON (UserF Maybe)) instance FromJSON (UserF Maybe)

instance Semigroup (UserF Maybe) where
  (<>) x y =
    User
      { userCommandAllowDir = userCommandAllowDir x <|> userCommandAllowDir y,
        userProjectConfig = userProjectConfig x <> userProjectConfig y
      }

instance Monoid (UserF Maybe) where
  mempty = User mempty mempty

-- | Resolve a 'UserF Maybe' value to a final 'UserF Identity' value.
--
-- @since 0.5.0.0
resolve :: forall m. MonadIO m => UserF Maybe -> m (UserF Identity)
resolve User {..} =
  User
    <$> maybe defaultAllowDir pure userCommandAllowDir
    <*> pure userProjectConfig
  where
    defaultAllowDir :: m FilePath
    defaultAllowDir =
      Directory.getXdgDirectory Directory.XdgData ("edify" </> "allow")
        & liftIO
