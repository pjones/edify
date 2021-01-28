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
module Edify.Compiler.Project
  ( ProjectF (..),
    Project,

    -- * Parsing Project Options
    fromCommandLine,

    -- * Size Hints for Assets
    SizeHints (..),

    -- * Formats and Targets
    Format (..),
    formatExtension,
    TargetF (..),
    Target,

    -- * Resolving a Final Value
    Error (..),
    resolve,
  )
where

import Control.Monad.Except (throwError)
import qualified Data.Char as Char
import Data.List ((\\))
import qualified Data.Text as Text
import qualified Edify.Compiler.FilePath as FilePath
import Edify.JSON
import Edify.Text.Indent (Tabstop (..), defaultTabstop)
import qualified Edify.Text.Placeholders as Placeholders
import qualified Options.Applicative as Opt
import qualified System.Directory as Directory

-- | Output formats.
--
-- @since 0.5.0.0
data Format
  = -- | Portal Document Format.
    PDF
  | -- | Hyper Text Markup Language.
    HTML
  deriving stock (Generic, Eq, Bounded, Enum)
  deriving (ToJSON, FromJSON) via GenericJSON Format

-- | Generate a file extension for the given 'Project.Format'.
--
-- @since 0.5.0.0
formatExtension :: Format -> FilePath.Ext
formatExtension = \case
  PDF -> FilePath.Ext "pdf"
  HTML -> FilePath.Ext "html"

-- | Desired image dimensions for formats that use raster images.
--
-- @since 0.5.0.0
data SizeHints = SizeHints
  { -- | Desired width.
    hintWidth :: Word,
    -- | Desired height.
    hintHeight :: Word
  }
  deriving stock (Generic)
  deriving (ToJSON, FromJSON) via GenericJSON SizeHints

-- | Default 'SizeHints'.
--
-- @since 0.5.0.0
defaultSizeHints :: SizeHints
defaultSizeHints = SizeHints 1024 768

-- | Description for documents to build out of Markdown.
--
-- @since 0.5.0.0
data TargetF (f :: Readiness) = Target
  { -- | A name for this target.  The first word
    -- in this name is used in the file extension when no explicit
    -- extension is set.
    targetName :: Text,
    -- | File extension used when generating files for this target.
    targetFileExtension :: Checked f (Maybe Text) FilePath.Ext,
    -- | The format for this target.  Used to compile assets to an
    -- appropriately matching format.
    targetFormat :: Format,
    -- | A shell command used to convert Markdown to the desired
    -- format.
    targetCommand :: Checked f Text ((FilePath, FilePath) -> Text)
  }
  deriving stock (Generic)

deriving via (GenericJSON (TargetF Parsed)) instance ToJSON (TargetF Parsed)

deriving via (GenericJSON (TargetF Parsed)) instance FromJSON (TargetF Parsed)

-- | A fully resolved 'TargetF'.
--
-- @since 0.5.0.0
type Target = TargetF Resolved

-- | Generate a file extension for the given 'Project.Target'.
--
-- @since 0.5.0.0
resolveTargetExtension :: TargetF Parsed -> Maybe FilePath.Ext
resolveTargetExtension Target {..} =
  case targetFileExtension of
    Nothing -> convert targetName
    Just ext -> convert ext
  where
    convert :: Text -> Maybe FilePath.Ext
    convert =
      Text.filter Char.isAlphaNum
        >>> Text.words
        >>> listToMaybe
        >>> fmap (FilePath.Ext . Text.toLower)

-- | Default targets if none are given.
--
-- @since 0.5.0.0
defaultTargets :: NonEmpty (TargetF Parsed)
defaultTargets =
  fromList
    [ Target
        { targetName = "handout",
          targetFileExtension = Nothing,
          targetFormat = PDF,
          targetCommand =
            command
              ( pandoc
                  <> [ "--to=latex",
                       "--toc",
                       "--toc-depth=2",
                       "--top-level-division=chapter",
                       "--number-sections",
                       "%i"
                     ]
              )
        },
      Target
        { targetName = "slides",
          targetFileExtension = Nothing,
          targetFormat = PDF,
          targetCommand =
            command
              ( pandoc
                  <> [ "--to=beamer",
                       "--slide-level=3",
                       "--variable=classoption:aspectratio=43",
                       "%i"
                     ]
              )
        },
      Target
        { targetName = "webpage",
          targetFileExtension = Nothing,
          targetFormat = HTML,
          targetCommand =
            command
              ( pandoc
                  <> [ "--to=html",
                       "--standalone",
                       "%i"
                     ]
              )
        }
    ]
  where
    command :: [Text] -> Text
    command = Text.intercalate " "

    pandoc :: [Text]
    pandoc =
      [ "pandoc",
        "--from=markdown",
        "--filter=pandoc-citeproc",
        "--output=%o"
      ]

-- | Project-level configuration.
--
-- @since 0.5.0.0
data ProjectF (f :: Readiness) = Project
  { -- | The directory where all generated files will be placed.
    projectOutputDirectory :: Default f FilePath,
    -- | The width of a tab character when converted to spaces.
    projectTabstop :: Default f Tabstop,
    -- | Size hints.
    projectSizeHints :: Default f SizeHints,
    -- | List of document targets.
    projectTargets :: Default f (NonEmpty (TargetF f)),
    -- | List of input Markdown files to process.
    projectInputFiles :: Default f (NonEmpty FilePath)
  }
  deriving stock (Generic)

instance Semigroup (ProjectF Parsed) where
  (<>) x y =
    Project
      { projectOutputDirectory =
          projectOutputDirectory x <|> projectOutputDirectory y,
        projectTabstop =
          projectTabstop x <|> projectTabstop y,
        projectSizeHints =
          projectSizeHints x <|> projectSizeHints y,
        projectTargets =
          projectTargets x <|> projectTargets y,
        projectInputFiles =
          projectInputFiles x <|> projectInputFiles y
      }

instance Monoid (ProjectF Parsed) where
  mempty =
    Project
      { projectOutputDirectory = Nothing,
        projectTabstop = Nothing,
        projectSizeHints = Nothing,
        projectTargets = Nothing,
        projectInputFiles = Nothing
      }

deriving via (GenericJSON (ProjectF Parsed)) instance ToJSON (ProjectF Parsed)

deriving via (GenericJSON (ProjectF Parsed)) instance FromJSON (ProjectF Parsed)

-- | A fully resolved 'ProjectF'.
--
-- @since 0.5.0.0
type Project = ProjectF Resolved

-- | Parse project configuration on the command line.
--
-- @since 0.5.0.0
fromCommandLine :: Opt.Parser (ProjectF Parsed)
fromCommandLine =
  Project
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
              Opt.short 'T',
              Opt.metavar "NUM",
              Opt.help "Change the width of a tab character"
            ]
      )
    <*> pure Nothing
    <*> pure Nothing
    <*> optional
      ( fromList -- Safe due to @some@.
          <$> some
            ( Opt.strArgument $
                mconcat
                  [ Opt.metavar "FILE ...",
                    Opt.help "Markdown files to process"
                  ]
            )
      )

-- | Error that may occur while resolving project configuration.
--
-- @since 0.5.0.0
data Error
  = -- | No input files were given.
    MissingInputFilesError
  | -- | Unable to create a valid file extension from a target
    -- configuration.
    InvalidTargetFileExtensionError Text
  | -- | Target command format string can't be parsed.
    TargetCommandInvalidFormatError Text String
  | -- | Target command format string contains invalid variables.
    TargetCommandInvalidVarsError Text
  deriving stock (Generic, Show)

-- | Resolve a 'Target' to its final value.
--
-- @since 0.5.0.0
resolveTarget :: Monad m => TargetF Parsed -> ExceptT Error m (TargetF Resolved)
resolveTarget target@Target {..} = do
  ext <-
    case resolveTargetExtension target of
      Nothing ->
        throwError
          ( InvalidTargetFileExtensionError $
              fromMaybe targetName targetFileExtension
          )
      Just ext -> pure ext

  cmd <- hoistEither (resolveCommand targetCommand)

  pure
    Target
      { targetName = targetName,
        targetFileExtension = ext,
        targetFormat = targetFormat,
        targetCommand = cmd
      }
  where
    resolveCommand :: Text -> Either Error ((FilePath, FilePath) -> Text)
    resolveCommand cmd =
      let boundVars = "io"
          freeVars ast = Placeholders.vars ast \\ boundVars
          toCommand ast (input, output) =
            Placeholders.substitute
              ( fromList
                  [ ('i', toText input),
                    ('o', toText output)
                  ]
              )
              ast
       in case Placeholders.parse cmd of
            Left s -> Left (TargetCommandInvalidFormatError cmd s)
            Right ast
              | null (freeVars ast) -> Right (toCommand ast)
              | otherwise -> Left (TargetCommandInvalidVarsError cmd)

-- | Resolve all values in a project configuration.
--
-- @since 0.5.0.0
resolve :: MonadIO m => ProjectF Parsed -> ExceptT Error m (ProjectF Resolved)
resolve Project {..} = do
  -- The list of input markdown files is mandatory and made absolute
  -- to the current directory which should already be the project
  -- input directory.
  files <-
    hoistEither (required MissingInputFilesError projectInputFiles)
      >>= traverse (liftIO . Directory.canonicalizePath)

  -- The list of targets is mandatory.  If missing we'll use the
  -- default targets.
  targets <-
    traverse resolveTarget $
      fromMaybe defaultTargets projectTargets

  -- The directory where output files are stored.
  output <-
    fromMaybe "build" projectOutputDirectory
      & Directory.canonicalizePath
      & liftIO

  pure
    Project
      { projectOutputDirectory = output,
        projectTabstop = fromMaybe defaultTabstop projectTabstop,
        projectSizeHints = fromMaybe defaultSizeHints projectSizeHints,
        projectTargets = targets,
        projectInputFiles = files
      }
  where
    required :: Error -> Maybe a -> Either Error a
    required e = \case
      Nothing -> Left e
      Just x -> Right x
