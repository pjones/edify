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
    Error (..),
    filePathToInput,
    filePathFromInput,
    makeFilePathAbsoluteTo,
    readInput,
  )
where

import qualified Byline as B
import qualified Data.Text.Lazy.IO as LText
import qualified System.Directory as Dir
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
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
    FromText Text
  deriving (Show)

instance B.ToStylizedText Input where
  toStylizedText = \case
    FromFile path ->
      "file " <> (B.text (toText path) <> B.fg B.green)
    FromHandle h ->
      (if h == stdin then "<stdin>" else B.text (show h))
        <> B.fg B.green
    -- B.text "input handle"
    FromText t ->
      "text:\n====\n"
        <> (B.text t <> B.fg B.green)
        <> "\n====\n"

-- | Errors that may occur while processing input.
--
-- @since 0.5.0.0
newtype Error
  = -- | The given file does not exist.
    FileDoesNotExist FilePath
  deriving (Generic, Show)

instance B.ToStylizedText Error where
  toStylizedText = \case
    FileDoesNotExist path ->
      mconcat
        [ "file does not exist ",
          B.text (toText path) <> B.fg B.magenta
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

-- | Make a 'FilePath' absolute when it is relative to another
-- 'FilePath'.
--
-- @since 0.5.0.0
makeFilePathAbsoluteTo ::
  -- | The existing absolute path of a file to compare to.
  FilePath ->
  -- | The relative path to make absolute.
  FilePath ->
  -- | The resolved file path.
  FilePath
makeFilePathAbsoluteTo context file
  | FilePath.isAbsolute file = file
  | otherwise =
    FilePath.normalise context
      & FilePath.takeDirectory
      & (</> FilePath.normalise file)

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
    pure (Right $ toLazy text)
