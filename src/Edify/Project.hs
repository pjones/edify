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
    resolve,
    defaultProjectConfig,
    readProjectConfigFile,
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
import qualified Edify.Compiler.User as User
import qualified Edify.Input as Input
import Edify.JSON
import qualified Edify.Project.Config as Config
import qualified Edify.Project.Error as Error
import qualified Edify.Project.Inputs as Inputs
import qualified Edify.Project.Target as Target
import qualified System.Directory as Directory

-- | A hybrid configuration and input file list.  This is the
-- configuration combination read from the local project configuration file.
--
-- @since 0.5.0.0
type ProjectConfig (f :: Readiness) = Config.ConfigF f :*: Inputs.InputsF f

-- | The default project when one hasn't been configured.
--
-- @since 0.5.0.0
defaultProjectConfig :: ProjectConfig Parsed
defaultProjectConfig = Join (Config.defaultConfig, mempty)

-- | Fully resolve and complete project configuration.
--
-- @since 0.5.0.0
data Project = Project
  { projectConfig :: Config.ConfigF Resolved,
    projectTopLevel :: Inputs.TopLevelF Resolved,
    projectInputs :: Inputs.InputsF Resolved
  }
  deriving stock (Generic)

-- | Resolve all components of a configuration into their final form.
--
-- Given the various components of a project configuration parsed from
-- the command line (or their default values), create a fully realized
-- composite configuration.
--
-- @since 0.5.0.0
resolve ::
  MonadIO m =>
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
resolve user cliTop cliInputs cliConfig = runExceptT $ do
  -- Figure out where the project actually lives:
  t <- Inputs.resolveTopLevel cliTop

  -- Try to read a local project configuration file:
  local <-
    readProjectConfigFile
      (user ^. #userCommandAllowDir)
      (t ^. #projectDirectory)
      >>= either (throwError . Error.ConfigInputError) pure

  let userConfig = user ^. #userProjectConfig
      localConfig = local ^. _1
      localInputs = local ^. _2

  -- Resolve remaining components:
  c <- Config.resolveConfig (cliConfig <> localConfig <> userConfig)
  i <- Inputs.resolveInputs (cliInputs <> localInputs)

  pure (Project c t i)

-- | Try to read a project-specific configuration file.
--
-- @since 0.5.0.0
readProjectConfigFile ::
  MonadIO m =>
  -- | Directory containing fingerprint files.
  FilePath ->
  -- | The project top-level directory.
  FilePath ->
  -- | The loaded configuration or an error.
  m (Either Input.Error (ProjectConfig Parsed))
readProjectConfigFile fpdir prjdir =
  let def = Right defaultProjectConfig
   in liftIO (Directory.doesDirectoryExist prjdir)
        >>= bool
          (pure def)
          ( Inputs.dirHasProjectConfig prjdir >>= \case
              Nothing ->
                pure def
              Just path ->
                Input.decodeFromFile
                  (Input.OnlyReadApprovedFiles fpdir projectCommands)
                  path
          )

-- | Get all commands from all targets.
--
-- @since 0.5.0.0
projectCommands :: ProjectConfig Parsed -> [Text]
projectCommands = (^.. _1 . #projectTargets . _Just . folded . #targetCommand)
