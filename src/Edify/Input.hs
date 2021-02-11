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
module Edify.Input
  ( Input (..),
    filePathToInput,
    filePathFromInput,
    readInput,

    -- * Encoding and Decoding Files
    ReadMode (..),
    decodeFromFile,
    WriteMode (..),
    encodeToFile,

    -- * Error Handling
    Error (..),
    renderInput,
    renderError,
  )
where

import Control.Monad.Except (throwError)
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy.IO as LText
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Yaml as YAML
import qualified Edify.Compiler.Fingerprint as Fingerprint
import qualified Prettyprinter.Render.Terminal as PP
import System.Directory (getCurrentDirectory)
import qualified System.Directory as Dir
import qualified System.FilePath as FilePath

-- | Where to read input from.
--
-- @since 0.5.0.0
data Input
  = -- | Read input from the given file.
    FromFile FilePath
  | -- | Read input from the given handle.
    FromHandle Handle
  | -- | Use the given 'Text' as input.
    FromText LText
  deriving (Show)

-- | Render an 'Input' value for displaying to the user.
--
-- @since 0.5.0.0
renderInput :: Input -> PP.Doc ann
renderInput = \case
  FromFile path ->
    "file " <> PP.pretty path
  FromHandle h ->
    if h == stdin
      then "<stdin>"
      else PP.pretty (show h :: Text)
  FromText t ->
    "text "
      <> PP.nest 4 (PP.line <> PP.pretty t)
      <> PP.line

-- | Errors that may occur while processing input.
--
-- @since 0.5.0.0
data Error
  = -- | The given file does not exist.
    FileDoesNotExist FilePath
  | -- | File exists and we don't want to overwrite it.
    FileWillBeOverritten FilePath
  | -- | Fingerprint doesn't match.
    FileNotApprovedForReading FilePath
  | -- | Can't parse a JSON file.
    FailedJsonParse FilePath String
  | -- | Can't parse a YAML file.
    FailedYamlParse FilePath YAML.ParseException
  deriving (Generic, Show)

-- | Render an error message to display to the user.
--
-- @since 0.5.0.0
renderError :: Error -> PP.Doc PP.AnsiStyle
renderError = \case
  FileDoesNotExist file ->
    fileError
      file
      [ PP.annotate (PP.color PP.Red) "does not exist"
      ]
  FileWillBeOverritten file ->
    fileError
      file
      [ PP.annotate (PP.color PP.Red) "would be overwritten"
      ]
  FileNotApprovedForReading file ->
    PP.vcat
      [ fileError
          file
          [ PP.annotate (PP.color PP.Red) "is not approved for reading",
            "because it may contain shell commands."
          ],
        "If the file is safe to read you can approve it with" <> PP.colon,
        PP.nest 4 (PP.line <> "edify allow " <> PP.pretty file)
      ]
  FailedJsonParse file msg ->
    fileError
      file
      [ "contains a JSON syntax error:",
        PP.annotate (PP.color PP.Red) (PP.pretty msg)
      ]
  FailedYamlParse file e ->
    fileError
      file
      [ "contains a YAML syntax error:",
        PP.annotate
          (PP.color PP.Red)
          (PP.pretty $ YAML.prettyPrintParseException e)
      ]
  where
    fileError :: FilePath -> [PP.Doc PP.AnsiStyle] -> PP.Doc PP.AnsiStyle
    fileError file msg =
      PP.vcat
        [ "file" <> PP.colon,
          PP.nest
            2
            ( mconcat
                [ PP.hardline,
                  PP.fillSep
                    ( PP.dquotes
                        (PP.annotate (PP.color PP.Yellow) (PP.pretty file)) :
                      msg
                    )
                ]
            )
            <> PP.hardline
        ]

-- | Figure out what kind of input a file path refers to.
--
-- @since 0.5.0.0
filePathToInput :: Maybe FilePath -> Input
filePathToInput = \case
  Nothing -> FromHandle stdin
  Just "-" -> FromHandle stdin
  Just file -> FromFile file

-- | Get a file path (or directory path) from an 'Input'.
--
-- If the input is a file, that file path is returned in 'Right'.
-- Otherwise the current directory is returned in 'Left'.
--
-- @since 0.5.0.0
filePathFromInput :: MonadIO m => Input -> m (Either FilePath FilePath)
filePathFromInput = \case
  FromFile path -> pure (Right path)
  FromHandle _ -> liftIO getCurrentDirectory <&> Left
  FromText _ -> liftIO getCurrentDirectory <&> Left

-- | Read and return input.
--
-- @since 0.5.0.0
readInput :: MonadIO m => Input -> m (Either Error LText)
readInput = \case
  FromFile path -> do
    exists <- liftIO (Dir.doesFileExist path)
    if exists
      then readFileLText path <&> Right
      else pure (Left $ FileDoesNotExist path)
  FromHandle h ->
    liftIO (LText.hGetContents h) <&> Right
  FromText text ->
    pure (Right text)

-- | How to deal with fingerprints when reading files.
--
-- @since 0.5.0.0
data ReadMode a
  = OnlyReadApprovedFiles FilePath (a -> [Text])
  | ReadWithoutFingerprint

-- | Decode the contents of a file (JSON or YAML).
--
-- @since 0.5.0.0
decodeFromFile ::
  forall m a.
  MonadIO m =>
  Aeson.FromJSON a =>
  ReadMode a ->
  FilePath ->
  m (Either Error a)
decodeFromFile mode file = do
  exists <- liftIO (Dir.doesFileExist file)
  if exists
    then go mode
    else pure (Left $ FileDoesNotExist file)
  where
    go :: ReadMode a -> m (Either Error a)
    go = \case
      OnlyReadApprovedFiles fpdir getCommands -> runExceptT $ do
        val <- ExceptT (readFileLBS file <&> parse)
        (fp :: Fingerprint.Fingerprint Fingerprint.Commands) <-
          Fingerprint.read mempty fpdir file
            >>= maybe (throwError $ FileNotApprovedForReading file) pure . fst
        let fps = Fingerprint.fingerprintContent fp
        case Fingerprint.verify (getCommands val) fps of
          Fingerprint.Verified -> pure val
          Fingerprint.Mismatch -> throwError $ FileNotApprovedForReading file
      ReadWithoutFingerprint ->
        readFileLBS file <&> parse
    parse :: LByteString -> Either Error a
    parse bytes = case FilePath.takeExtension file of
      ".json" ->
        Aeson.eitherDecode bytes
          & first (FailedJsonParse file)
      _yaml ->
        YAML.decodeEither' (toStrict bytes)
          & first (FailedYamlParse file)

-- | How to deal with fingerprints with writing files.
--
-- @since 0.5.0.0
data WriteMode a
  = WriteFingerprintTo FilePath (a -> [Text])
  | WriteWithoutFingerprint

-- | Encode a value (JSON/YAML) then write it to a file.
--
-- @since 0.5.0.0
encodeToFile ::
  MonadIO m =>
  Aeson.ToJSON a =>
  -- | How to deal with fingerprints.
  WriteMode a ->
  -- | The file to write.
  FilePath ->
  -- | The value to encode.
  a ->
  -- | Unit or error.
  m (Either Error ())
encodeToFile mode file x = runExceptT $ do
  whenM (liftIO (Dir.doesFileExist file)) $
    throwError (FileWillBeOverritten file)

  let bytes = case FilePath.takeExtension file of
        ".json" -> Aeson.encode x
        _yaml -> toLazy (YAML.encode x)

  writeFileLBS file bytes

  case mode of
    WriteWithoutFingerprint -> pass
    WriteFingerprintTo dir getCommands ->
      let cmds = Fingerprint.generate (getCommands x)
          cache = Fingerprint.cache file (cmds :: Fingerprint.Commands) mempty
       in Fingerprint.write dir cache
