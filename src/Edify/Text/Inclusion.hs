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
-- General purpose method for injecting one file into another.
module Edify.Text.Inclusion
  ( Chunk (..),
    Error (..),
    toChunks,
    fromChunks,
  )
where

import qualified Byline as B
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as Text
import Edify.JSON

-- | The input text translated into chunks.
--
-- @since 0.5.0.0
data Chunk
  = -- | A chunk of text from the input.
    Chunk Text
  | -- | A request to include another file.  The included file can
    -- also be narrowed to the given token or section ID.
    Include FilePath (Maybe Text)
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via GenericJSON Chunk

-- | Types of errors that may occur while parsing a file for
-- inclusions.
--
-- @since 0.5.0.0
newtype Error = Error String
  deriving (Show, Eq)

instance B.ToStylizedText Error where
  toStylizedText (Error s) =
    mconcat
      [ "unable to parse text for inclusion markers: ",
        B.text (toText s) <> B.fg B.magenta
      ]

-- | Convert a 'Text' value into a list of chunks.
--
-- @since 0.5.0.0
toChunks :: Text -> Either Error [Chunk]
toChunks =
  Atto.parseOnly
    ( many
        ( includeP
            <|> emptyLinesP
            <|> chunkP
        )
        <* Atto.endOfInput
    )
    >>> first Error
  where
    includeP =
      include
        <&> uncurry Include
    emptyLinesP =
      Atto.many1 (Atto.satisfy Atto.isEndOfLine)
        <&> (toText >>> Chunk)
    chunkP = do
      chars <- Atto.many1 (Atto.satisfy (Atto.isEndOfLine >>> not))
      ends <- many (Atto.satisfy Atto.isEndOfLine)
      pure (Chunk $ toText chars <> toText ends)

-- | Convert a list of chunks back into a 'Text' value.
--
-- @since 0.5.0.0
fromChunks :: [Chunk] -> Text
fromChunks = foldMap $ \case
  Chunk t -> t
  Include path token ->
    "<<("
      <> toText path
      <> maybe mempty ("#" <>) token
      <> ")\n"

-- | Parse an inclusion marker and separate it into a file path and
-- token.
--
-- @since 0.5.0.0
include :: Atto.Parser (FilePath, Maybe Text)
include = do
  Atto.skipWhile Atto.isHorizontalSpace
  content <-
    Atto.string "<<("
      *> Atto.many1 (Atto.satisfy (/= ')'))
      <* Atto.char ')'
  Atto.skipWhile Atto.isHorizontalSpace
  Atto.endOfLine <|> Atto.endOfInput
  Text.span (/= '#') (toText content)
    & bimap toString (Text.stripPrefix "#")
    & pure
