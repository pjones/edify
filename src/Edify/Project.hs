{-# LANGUAGE TemplateHaskell #-}

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
module Edify.Project
  ( ProjectConfig,
    Project (..),
    ReadMode (..),
    resolve,
    defaultProjectConfigBytes,
    defaultProjectConfig,
    projectCommands,

    -- * Re-exports
    Error.Error (..),
    Error.renderError,
    Config.ConfigF (..),
    Config.Config,
    Config.configFromCommandLine,
    Config.resolveConfig,
    Inputs.InputsF (..),
    Inputs.Inputs,
    Inputs.defaultProjectConfigFile,
    Inputs.projectConfigFiles,
    Inputs.inputsFromCommandLine,
    Inputs.resolveInputs,
    Inputs.TopLevelF (..),
    Inputs.TopLevel,
    Inputs.topLevelFromCommandLine,
    Inputs.resolveTopLevel,
    Target.TargetF (..),
    Target.Target,
    Target.formatExtension,
    Target.Format (..),
    Config.SizeHints (..),
  )
where

import Control.Lens (folded, (^.), (^..), _1, _2, _Just)
import Control.Monad.Except (throwError)
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Yaml as YAML
import qualified Edify.Compiler.User as User
import qualified Edify.Project.Config as Config
import qualified Edify.Project.Error as Error
import qualified Edify.Project.Inputs as Inputs
import qualified Edify.Project.Target as Target
import qualified Edify.System.Input as Input
import Edify.Text.JSON
import qualified System.Directory as Directory

-- | A hybrid configuration and input file list.  This is the
-- configuration combination read from the local project configuration file.
--
-- @since 0.5.0.0
type ProjectConfig (f :: Readiness) = Config.ConfigF f :*: Inputs.InputsF f

-- | The default configuration as a YAML encoded 'ByteString'.
--
-- @since 0.5.0.0
defaultProjectConfigBytes :: ByteString
defaultProjectConfigBytes =
  $( FileEmbed.makeRelativeToProject "data/config/project.yml"
       >>= FileEmbed.embedFile
   )

-- | The default project when one hasn't been configured.
--
-- @since 0.5.0.0
defaultProjectConfig :: ProjectConfig Parsed
defaultProjectConfig =
  fromRight
    (error "impossible")
    (YAML.decodeEither' defaultProjectConfigBytes)

-- | Fully resolve and complete project configuration.
--
-- @since 0.5.0.0
data Project = Project
  { projectConfig :: Config.ConfigF Resolved,
    projectTopLevel :: Inputs.TopLevelF Resolved,
    projectInputs :: Inputs.InputsF Resolved,
    projectReadFrom :: Maybe FilePath
  }
  deriving stock (Generic)

-- | How safe to be when reading project configuration files.
--
-- @since 0.5.0.0
data ReadMode
  = -- | Require commands to be approved.
    OnlyReadApproved
  | -- | Read without consulting fingerprints.
    ReadWithoutFingerprint

-- | Convert read mode to the type needed by the "Input" module.
toInputReadMode :: User.User -> ReadMode -> Input.ReadMode (ProjectConfig Parsed)
toInputReadMode user = \case
  OnlyReadApproved ->
    Input.OnlyReadApprovedFiles (user ^. #userCommandAllowDir) projectCommands
  ReadWithoutFingerprint ->
    Input.ReadWithoutFingerprint

-- | Resolve all components of a configuration into their final form.
--
-- Given the various components of a project configuration parsed from
-- the command line (or their default values), create a fully realized
-- composite configuration.
--
-- @since 0.5.0.0
resolve ::
  MonadIO m =>
  -- | Safety level.
  ReadMode ->
  -- | Fully resolved user configuration.
  User.User ->
  -- | Command line options or 'mempty'.
  Inputs.TopLevelF Parsed ->
  -- | Command line options or 'mempty'.
  Inputs.InputsF Parsed ->
  -- | Command line options or 'mempty'.
  Config.ConfigF Parsed ->
  -- | Fully resolved project configuration or error.
  m (Either Error.Error Project)
resolve mode user cliTop cliInputs cliConfig = runExceptT $ do
  -- Figure out where the project actually lives:
  t <- Inputs.resolveTopLevel cliTop

  -- Try to read a local project configuration file:
  (local, path) <-
    readProjectConfigFile
      (toInputReadMode user mode)
      (t ^. #projectDirectory)
      >>= either (throwError . Error.ConfigInputError) pure

  let userConfig = user ^. #userProjectConfig
      localConfig = local ^. _1
      defConfig = defaultProjectConfig ^. _1
      localInputs = local ^. _2

  -- Resolve remaining components:
  c <- Config.resolveConfig (cliConfig <> localConfig <> userConfig <> defConfig)
  i <- Inputs.resolveInputs (cliInputs <> localInputs)

  pure (Project c t i path)

-- | Try to read a project-specific configuration file.
readProjectConfigFile ::
  MonadIO m =>
  -- | Whether or not to use fingerprints.
  Input.ReadMode (ProjectConfig Parsed) ->
  -- | The project top-level directory.
  FilePath ->
  -- | The loaded configuration and the file it was read from.
  m (Either Input.Error (ProjectConfig Parsed, Maybe FilePath))
readProjectConfigFile mode dir =
  let def = Right (defaultProjectConfig, Nothing)
   in liftIO (Directory.doesDirectoryExist dir)
        >>= bool
          (pure def)
          ( Inputs.dirHasProjectConfig dir >>= \case
              Nothing ->
                pure def
              Just path ->
                Input.decodeFromFile mode path
                  <&> second (,Just path)
          )

-- | Get all commands from all targets.
--
-- @since 0.5.0.0
projectCommands :: ProjectConfig Parsed -> [Text]
projectCommands = (^.. _1 . #projectTargets . _Just . folded . #targetCommand)
