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
module Edify.Project.Config
  ( ConfigF (..),
    Config,
    SizeHints (..),
    defaultSizeHints,
    configFromCommandLine,
    resolveConfig,
  )
where

import Control.Monad.Except (throwError)
import Edify.JSON
import qualified Edify.Project.Error as Error
import qualified Edify.Project.Target as Target
import Edify.Text.Indent (Tabstop (..), defaultTabstop)
import qualified Generics.SOP as SOP
import qualified Options.Applicative as Opt
import qualified System.Directory as Directory

-- | Desired image dimensions for formats that use raster images.
--
-- @since 0.5.0.0
data SizeHints = SizeHints
  { -- | Desired width.
    hintWidth :: Word,
    -- | Desired height.
    hintHeight :: Word
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (ToJSON, FromJSON) via GenericJSON SizeHints

-- | Default 'SizeHints'.
--
-- @since 0.5.0.0
defaultSizeHints :: SizeHints
defaultSizeHints = SizeHints 1024 768

-- | Project-level configuration.
--
-- @since 0.5.0.0
data ConfigF (f :: Readiness) = Config
  { -- | The directory where all generated files will be placed.
    projectOutputDirectory :: Default f FilePath,
    -- | The width of a tab character when converted to spaces.
    projectTabstop :: Default f Tabstop,
    -- | Size hints.
    projectSizeHints :: Default f SizeHints,
    -- | List of document targets.
    projectTargets :: Default f (NonEmpty (Target.TargetF f))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Semigroup (ConfigF Parsed) where
  (<>) x y =
    Config
      { projectOutputDirectory =
          projectOutputDirectory x <|> projectOutputDirectory y,
        projectTabstop =
          projectTabstop x <|> projectTabstop y,
        projectSizeHints =
          projectSizeHints x <|> projectSizeHints y,
        projectTargets =
          projectTargets x <|> projectTargets y
      }

instance Monoid (ConfigF Parsed) where
  mempty =
    Config
      { projectOutputDirectory = Nothing,
        projectTabstop = Nothing,
        projectSizeHints = Nothing,
        projectTargets = Nothing
      }

deriving via (GenericJSON (ConfigF Parsed)) instance ToJSON (ConfigF Parsed)

deriving via (GenericJSON (ConfigF Parsed)) instance FromJSON (ConfigF Parsed)

deriving instance Eq (ConfigF Parsed)

deriving instance Show (ConfigF Parsed)

-- | Resolved configurations.
--
-- @since 0.5.0.0
type Config = ConfigF Resolved

-- | Parse project configuration on the command line.
--
-- @since 0.5.0.0
configFromCommandLine :: Opt.Parser (ConfigF Parsed)
configFromCommandLine =
  Config
    <$> optional
      ( Opt.strOption $
          mconcat
            [ Opt.long "output",
              Opt.short 'o',
              Opt.metavar "DIR",
              Opt.help "Project output directory directory"
            ]
      )
    <*> optional
      ( Opt.option (Tabstop <$> Opt.auto) $
          mconcat
            [ Opt.long "tabstop",
              Opt.metavar "NUM",
              Opt.help "Change the width of a tab character"
            ]
      )
    <*> pure Nothing
    <*> pure Nothing

-- | Resolve all values in a project configuration.
--
-- @since 0.5.0.0
resolveConfig ::
  MonadIO m =>
  ConfigF Parsed ->
  ExceptT Error.Error m (ConfigF Resolved)
resolveConfig Config {..} = do
  -- The list of targets is mandatory.
  targets <-
    case projectTargets of
      Nothing -> throwError Error.MissingTargetsError
      Just targets -> traverse Target.resolve targets

  case Target.targetsByFileExtension targets of
    Right _targetMap -> pass
    Left (dup0, dup1) ->
      throwError $
        Error.TargetWithDuplicateFileExtensionError
          (Target.targetName dup0)
          (Target.targetName dup1)
          (Target.targetFileExtension dup0)

  -- The directory where output files are stored.
  output <-
    fromMaybe "build" projectOutputDirectory
      & Directory.canonicalizePath
      & liftIO

  pure
    Config
      { projectOutputDirectory = output,
        projectTabstop = fromMaybe defaultTabstop projectTabstop,
        projectSizeHints = fromMaybe defaultSizeHints projectSizeHints,
        projectTargets = targets
      }
