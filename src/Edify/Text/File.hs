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
module Edify.Text.File
  ( Config (..),
    Input (..),
    Error (..),
    processInput,
  )
where

import Control.Monad.Except
import qualified Data.Text.IO as Text
import qualified Edify.Text.Indent as Indent
import qualified Edify.Text.Narrow as Narrow
import qualified System.Directory as Dir

-- | How to process a file.
--
-- @since 0.5.0.0
data Config = Config
  { -- | If a token is give, narrow the input file.
    configNarrow :: Maybe Narrow.Token,
    -- | Whether or not to remove leading indentation.
    configStripIndentation :: Bool
  }

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

-- | Errors that may occur while processing a file.
--
-- @since 0.5.0.0
data Error
  = -- | The given file does not exist.
    FileDoesNotExist FilePath
  | -- | An error occured while narrwing the text.
    NarrowError Narrow.Error
  deriving (Show, Eq)

-- | Read 'Text' from the given 'Input' and process it according to
-- 'Config'.
--
-- @since 0.5.0.0
processInput :: MonadIO m => Config -> Input -> m (Either Error Text)
processInput Config {..} input = runExceptT $ do
  content <- case input of
    FromFile path -> do
      unlessM (liftIO (Dir.doesFileExist path)) $
        throwError (FileDoesNotExist path)
      readFileText path
    FromHandle h ->
      liftIO (Text.hGetContents h)
    FromText text ->
      pure text
  ExceptT (pure (narrow content >>= stripIndentation))
  where
    narrow :: Text -> Either Error Text
    narrow text = case configNarrow of
      Nothing ->
        pure text
      Just token ->
        Narrow.narrow token text
          & first NarrowError
    stripIndentation :: Text -> Either Error Text
    stripIndentation text
      | configStripIndentation = pure (Indent.stripLeadingIndent text)
      | otherwise = pure text
