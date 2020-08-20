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
--
-- Options that control the behavior of the project building process.
module Edify.Project.Options
  ( OptionsF (..),
    Options,
    fromCommandLine,
    fromFile,
    resolve,
  )
where

import Control.Monad.Except (MonadError, throwError)
import qualified Data.Aeson as Aeson
import qualified Data.YAML as YAML
import qualified Data.YAML.Aeson as JAML
import Edify.JSON
import qualified Options.Applicative as O
import System.Directory (XdgDirectory (..), doesFileExist, getXdgDirectory)
import System.FilePath ((<.>), (</>), hasExtension, takeExtension)
import qualified Text.Pandoc.App as Pandoc

-- | A type family for values that need to be resolved at run time.
--
-- When the @f@ argument is 'Maybe' then the type resolves to @Maybe a@.
--
-- When the @f@ argument is 'Identity' then the type resolves to @b@.
type family Resolve (f :: * -> *) a b where
  Resolve Maybe a b = Maybe a
  Resolve Identity a b = b

-- | A type family for types that have a default value.
--
-- When the @f@ argument is 'Maybe', the value is wrapped in a 'Maybe'.
--
-- When the @f@ arguments is 'Identity' the type is exposed directly.
type family Optional (f :: * -> *) a where
  Optional Maybe a = Maybe a
  Optional Identity a = a

-- | Options that control how a project will be processed.
--
-- @since 0.5.0.0
data OptionsF f = Options
  { -- | The name, basename, or absolute name of a Pandoc defaults file.
    --
    -- Once the file options file has been read and processed this
    -- will be a full Pandoc 'Pandoc.Opt' value instead.
    optionsPandocDefaults :: Resolve f FilePath Pandoc.Opt,
    -- | The directory where all generated files will be placed.
    optionsOutputDirectory :: Optional f FilePath,
    -- | The name of the directory holding metadata files.
    optionsMetadataDirectory :: Optional f FilePath
  }
  deriving stock (Generic)

-- | The 'OptionsF' type fully resolved.
--
-- @since 0.5.0.0
type Options = OptionsF Identity

deriving via (GenericJSON (OptionsF Maybe)) instance ToJSON (OptionsF Maybe)

deriving via (GenericJSON (OptionsF Maybe)) instance FromJSON (OptionsF Maybe)

instance Semigroup (OptionsF Maybe) where
  (<>) x y =
    Options
      { optionsPandocDefaults =
          optionsPandocDefaults x <|> optionsPandocDefaults y,
        optionsOutputDirectory =
          optionsOutputDirectory x <|> optionsOutputDirectory y,
        optionsMetadataDirectory =
          optionsMetadataDirectory x <|> optionsMetadataDirectory y
      }

instance Monoid (OptionsF Maybe) where
  mempty =
    Options
      { optionsPandocDefaults = Nothing,
        optionsOutputDirectory = Nothing,
        optionsMetadataDirectory = Nothing
      }

-- | Errors that may occur.
--
-- @since 0.5.0.0
data Error
  = PandocDefaultsFileMissingError FilePath
  | PandocDefaulsLoadError FilePath String
  | OptionsFileDoesNotExistError FilePath
  | OptionsFileLoadError FilePath String

-- | Command line parser.
--
-- @since 0.5.0.0
fromCommandLine :: O.Parser (OptionsF Maybe)
fromCommandLine =
  Options
    <$> optional
      ( O.strOption $
          mconcat
            [ O.long "pandoc-defaults",
              O.short 'd',
              O.metavar "FILE",
              O.help "Find and load FILE as a Pandoc defaults file"
            ]
      )
    <*> optional
      ( O.strOption $
          mconcat
            [ O.long "output-directory",
              O.short 'o',
              O.metavar "DIR",
              O.help "Place all generated files in DIR"
            ]
      )
    <*> optional
      ( O.strOption $
          mconcat
            [ O.long "metadata-directory",
              O.short 'm',
              O.metavar "DIR",
              O.help "Load metadata files from DIR"
            ]
      )

-- | Loading options from a file.
--
-- @since 0.5.0.0
fromFile ::
  forall m.
  MonadIO m =>
  MonadError Error m =>
  FilePath ->
  m (OptionsF Maybe)
fromFile (defaultExtension "yaml" -> file) = do
  unlessM (liftIO $ doesFileExist file) $
    throwError (OptionsFileDoesNotExistError file)
  case takeExtension file of
    ".json" -> fromJSON file
    _ -> fromYAML file
  where
    -- Load options from YAML via the FromJSON instance.
    fromYAML :: FilePath -> m (OptionsF Maybe)
    fromYAML file =
      readFileLBS file
        >>= ( JAML.decode1 >>> \case
                Left (_, msg) -> throwError (OptionsFileLoadError file msg)
                Right opt -> pure opt
            )
    -- Load options from JSON via the FromJSON instance.
    fromJSON :: FilePath -> m (OptionsF Maybe)
    fromJSON file =
      readFileLBS file
        >>= ( Aeson.eitherDecode >>> \case
                Left msg -> throwError (OptionsFileLoadError file msg)
                Right opt -> pure opt
            )

-- | Resolve missing values, setting them to their defaults.
--
-- @since 0.5.0.0
resolve ::
  forall m.
  MonadIO m =>
  MonadError Error m =>
  OptionsF Maybe ->
  m Options
resolve Options {..} = do
  let metadataDir = fromMaybe "metadata" optionsMetadataDirectory
  pandocDefaults <- loadPandocDefaults metadataDir optionsPandocDefaults
  pure $
    Options
      { optionsPandocDefaults = pandocDefaults,
        optionsOutputDirectory = fromMaybe "build" optionsOutputDirectory,
        optionsMetadataDirectory = metadataDir
      }

-- | Return a 'Pandoc.Opt' value by either loading a file or using the
-- default values.
--
-- @since 0.5.0.0
loadPandocDefaults ::
  forall m.
  MonadIO m =>
  MonadError Error m =>
  -- | The metadata directory to search first.
  FilePath ->
  -- | The name of the defaults.  'Nothing' means to load the
  -- default values.
  Maybe String ->
  -- | The loaded 'Pandoc.Opt' or an 'Error'.
  m Pandoc.Opt
loadPandocDefaults metadata = \case
  Nothing -> pure Pandoc.defaultOpts
  Just name -> do
    let file = defaultExtension "yaml" name
    dirs <- (\pd -> [metadata, pd]) <$> pandocDefaultsDirectory
    match <- traverse (fileExistsInDir file) dirs <&> asum
    case match of
      Nothing -> throwError (PandocDefaultsFileMissingError file)
      Just path -> decodeFile path
  where
    decodeFile :: FilePath -> m Pandoc.Opt
    decodeFile path =
      readFileLBS path
        >>= ( YAML.decode1 >>> \case
                Left (_, msg) -> throwError (PandocDefaulsLoadError path msg)
                Right f -> pure (f Pandoc.defaultOpts)
            )
    fileExistsInDir :: FilePath -> FilePath -> m (Maybe FilePath)
    fileExistsInDir file dir =
      let path = dir </> file
       in liftIO (doesFileExist path)
            <&> bool Nothing (Just path)
    pandocDefaultsDirectory :: m FilePath
    pandocDefaultsDirectory =
      liftIO $
        getXdgDirectory XdgData "pandoc"
          <&> (</> "defaults")

-- | If a file name is missing an extension, add it.
--
-- @since 0.5.0.0
defaultExtension :: String -> FilePath -> FilePath
defaultExtension ext file
  | hasExtension file = file
  | otherwise = file <.> ext
