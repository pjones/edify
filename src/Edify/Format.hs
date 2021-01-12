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
module Edify.Format
  ( Format (..),
    Error (..),
    Token (..),
    defaultFormat,
    markdown,
    fromFileExtension,
  )
where

import qualified Byline as B
import qualified Edify.Format.Markdown as Markdown
import Edify.Text.Narrow (Token (..))
import qualified Edify.Text.Narrow as Narrow
import System.FilePath (takeExtension)

-- | Errors that can occur.
--
-- @since 0.5.0.0
data Error
  = NarrowError Narrow.Error
  | MarkdownError Markdown.Error
  deriving (Generic, Show)

instance B.ToStylizedText Error where
  toStylizedText = \case
    NarrowError e -> B.toStylizedText e
    MarkdownError e -> B.toStylizedText e

-- | Functions table for various formats.
--
-- @since 0.5.0.0
newtype Format = Format
  { narrow :: Token -> LText -> Either Error LText
  }

-- | The default function table.
--
-- @since 0.5.0.0
defaultFormat :: Format
defaultFormat = Format {..}
  where
    narrow :: Token -> LText -> Either Error LText
    narrow token input =
      Narrow.narrow token (toStrict input)
        & bimap NarrowError toLazy

-- | The function table for Markdown documents.
--
-- @since 0.5.0.0
markdown :: Format
markdown = Format {..}
  where
    narrow :: Token -> LText -> Either Error LText
    narrow token input =
      Markdown.narrow token input
        & first MarkdownError

-- | Get the function table for a file based on its extension.
--
-- @since 0.5.0.0
fromFileExtension :: FilePath -> Format
fromFileExtension =
  takeExtension >>> \case
    ".md" -> markdown
    ".markdown" -> markdown
    _ -> defaultFormat
