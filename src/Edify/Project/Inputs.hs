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
module Edify.Project.Inputs
  ( InputsF (..),
    Inputs,
    inputsFromCommandLine,
    resolveInputs,
    projectConfigFiles,
    defaultProjectConfigFile,
    dirHasProjectConfig,

    -- * The Project Input Directory
    TopLevelF (..),
    TopLevel,
    topLevelFromCommandLine,
    resolveTopLevel,
  )
where

import Control.Monad.Except (throwError)
import Edify.Compiler.FilePath ((</>))
import qualified Edify.Compiler.FilePath as FilePath
import Edify.JSON
import qualified Edify.Project.Error as Error
import qualified Options.Applicative as Opt
import qualified System.Directory as Directory
import qualified System.PosixCompat as Posix

-- | FIXME: Write documentation for ProjectInputsF (f :: Readiness)
--
-- @since 0.5.0.0
newtype InputsF (f :: Readiness) = Inputs
  { -- | List of input Markdown files to process.
    projectInputFiles :: Default f (NonEmpty FilePath)
  }
  deriving stock (Generic)

instance Semigroup (InputsF Parsed) where
  (<>) x y =
    Inputs
      { projectInputFiles =
          projectInputFiles x <|> projectInputFiles y
      }

instance Monoid (InputsF Parsed) where
  mempty =
    Inputs
      { projectInputFiles = Nothing
      }

deriving via (GenericJSON (InputsF Parsed)) instance ToJSON (InputsF Parsed)

deriving via (GenericJSON (InputsF Parsed)) instance FromJSON (InputsF Parsed)

-- | FIXME: Write documentation for Inputs
--
-- @since 0.5.0.0
type Inputs = InputsF Resolved

-- | Parse project inputs from the command line.
--
-- @since 0.5.0.0
inputsFromCommandLine :: Opt.Parser (InputsF Parsed)
inputsFromCommandLine =
  Inputs
    <$> optional
      ( fromList -- Safe due to @some@.
          <$> some
            ( Opt.strArgument $
                mconcat
                  [ Opt.metavar "FILE ...",
                    Opt.help "Markdown files to process"
                  ]
            )
      )

-- | FIXME: Write documentation for TopLevelF
--
-- @since 0.5.0.0
data TopLevelF (f :: Readiness) = TopLevel
  { projectDirectory :: Default f FilePath,
    projectInitialDirectory :: Checked f () FilePath
  }
  deriving stock (Generic)

instance Semigroup (TopLevelF Parsed) where
  (<>) x y =
    TopLevel
      { projectDirectory = projectDirectory x <|> projectDirectory y,
        projectInitialDirectory = projectInitialDirectory x
      }

instance Monoid (TopLevelF Parsed) where
  mempty =
    TopLevel
      { projectDirectory = Nothing,
        projectInitialDirectory = ()
      }

deriving via
  (GenericJSON (TopLevelF Parsed))
  instance
    ToJSON (TopLevelF Parsed)

deriving via
  (GenericJSON (TopLevelF Parsed))
  instance
    FromJSON (TopLevelF Parsed)

-- | FIXME: Write documentation for TopLevel
--
-- @since 0.5.0.0
type TopLevel = TopLevelF Resolved

-- | FIXME: Write description for topLevelFromCommandLine
--
-- @since 0.5.0.0
topLevelFromCommandLine :: Opt.Parser (TopLevelF Parsed)
topLevelFromCommandLine =
  TopLevel
    <$> optional
      ( Opt.strOption $
          mconcat
            [ Opt.long "top",
              Opt.short 'T',
              Opt.metavar "DIR",
              Opt.help "Use DIR as the top-level project directory"
            ]
      )
    <*> pass

-- | List of supported file names for project configuration.
--
-- @since 0.5.0.0
projectConfigFiles :: NonEmpty FilePath
projectConfigFiles =
  fromList
    [ ".edify.yml",
      ".edify.yaml",
      ".edify.json"
    ]

-- | The base name of the project configuration file.
--
-- @since 0.5.0.0
defaultProjectConfigFile :: FilePath
defaultProjectConfigFile = head projectConfigFiles

-- | Search for a project configuration file by walking up the file
-- system hierarchy.  Returns the project directory and a path to the
-- project configuration file.
--
-- @since 0.5.0.0
findProjectDirectory :: forall m. MonadIO m => m (Maybe FilePath)
findProjectDirectory = liftIO Directory.getCurrentDirectory >>= go
  where
    go :: FilePath -> m (Maybe FilePath)
    go dir =
      dirHasProjectConfig dir >>= \case
        Just _file -> pure (Just dir)
        Nothing -> continue go dir

    continue :: Alternative f => (FilePath -> m (f a)) -> FilePath -> m (f a)
    continue f dir
      | FilePath.isDrive dir = pure empty
      | otherwise = do
        let parent = FilePath.takeDirectory dir
            owner path = liftIO (Posix.getFileStatus path) <&> Posix.fileOwner
        sameOwners <- (==) <$> owner dir <*> owner parent
        if sameOwners
          then f parent
          else pure empty

-- | If the given directory path contains a project configuration
-- file, return the complete path to that file.
--
-- @since 0.5.0.0
dirHasProjectConfig :: MonadIO m => FilePath -> m (Maybe FilePath)
dirHasProjectConfig dir =
  runMaybeT $
    asumMap
      ( \file -> do
          let path = dir </> file
          exists <- liftIO (Directory.doesFileExist path)
          if exists then pure path else empty
      )
      projectConfigFiles

-- | Resolve project inputs.
--
-- @since 0.5.0.0
resolveInputs ::
  MonadIO m =>
  InputsF Parsed ->
  ExceptT Error.Error m (InputsF Resolved)
resolveInputs Inputs {..} = do
  files <-
    maybe (throwError Error.MissingInputFilesError) pure projectInputFiles
      >>= traverse (liftIO . Directory.canonicalizePath)

  pure
    Inputs
      { projectInputFiles = files
      }

-- | Resolve missing values, setting them to their defaults.
--
-- @since 0.5.0.0
resolveTopLevel ::
  forall m.
  MonadIO m =>
  TopLevelF Parsed ->
  m (TopLevelF Resolved)
resolveTopLevel TopLevel {..} = do
  cwd <- liftIO Directory.getCurrentDirectory
  top <- locateTopLevelDir
  liftIO (Directory.setCurrentDirectory top)

  pure
    TopLevel
      { projectDirectory = top,
        projectInitialDirectory = cwd
      }
  where
    locateTopLevelDir :: m FilePath
    locateTopLevelDir =
      let choices =
            -- Pick the first successful of these:
            [ maybe empty pure projectDirectory,
              MaybeT findProjectDirectory
            ]
       in runMaybeT (asum choices)
            >>= maybe (liftIO Directory.getCurrentDirectory) pure
            >>= liftIO . Directory.canonicalizePath
