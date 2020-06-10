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
-- Crop text based on starting and ending markers.
module Edify.Text.Narrow
  ( Markers (..),
    Token (..),
    NarrowError (..),
    narrow,
    narrowWith,
    narrowP,
  )
where

import qualified Data.Attoparsec.Text as Atto
import Data.Char (isSpace)
import qualified Data.Text as Text

-- | Beginning and ending markers for narrowing.
--
-- @since 0.5.0.0
data Markers = Markers Text Text
  deriving (Show, Eq)

-- | A bit of text that identifies a specific marker.
--
-- @since 0.5.0.0
newtype Token = Token
  {unToken :: Text}
  deriving (Show, Eq)

-- | Types of errors that may occur while narrowing.
--
-- @since 0.5.0.0
newtype NarrowError = NarrowError String
  deriving (Show, Eq)

-- | Narrow the given text using the default token delimiters.
--
-- @since 0.5.0.0
narrow :: Token -> Text -> Either NarrowError Text
narrow = narrowWith (Markers "<<:" ":>>")

-- | Narrow the given text to a beginning and ending delimiter.
--
-- @since 0.5.0.0
narrowWith :: Markers -> Token -> Text -> Either NarrowError Text
narrowWith m t input =
  case Atto.parseOnly (narrowP m t) input of
    Left e -> Left (NarrowError e)
    Right text -> Right text

-- | Parser that extracts the text between two markers that are
-- identified with a name ('Token').   Only consumes enough input to
-- extract the ending marker.
--
-- @since 0.5.0.0
narrowP :: Markers -> Token -> Atto.Parser Text
narrowP (Markers mstart mend) (Token token) = do
  let openP = do
        _ <- Atto.string mstart
        _ <- Atto.many1 Atto.space
        _ <- Atto.string token
        c <- Atto.peekChar'
        if isSpace c
          then -- Found the opening marker and token.
            pure ()
          else -- Found token with same prefix, backtrack.
            empty
      closeP = do
        _ <- Atto.string mend
        c <- Atto.peekChar' <|> pure '\n'
        if isSpace c
          then -- Found the closing marker.
            pure ()
          else -- Looks like the closing marker, except that the
          -- character after the marker isn't a space or EOF.
            empty

  -- Skip over all characters until we hit the starting marker and the
  -- token.
  _ <-
    Atto.manyTill Atto.anyChar openP
      Atto.<?> ( "opening marker ("
                   <> toString mstart
                   <> " "
                   <> toString token
                   <> ")"
               )

  -- Skill all other characters on the current line.
  _ <- Atto.manyTill Atto.anyChar Atto.endOfLine

  -- Now fetch all the characters *before* the ending marker.  Also
  -- skip the entire line that the ending marker is on.
  Atto.manyTill Atto.anyChar closeP
    <&> (toText >>> Text.dropWhileEnd (/= '\n'))
    Atto.<?> ("closing marker (" <> toString mend <> ")")
