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
module Edify.Markdown.Common
  ( matchingBracketP,
    quotedTextP,
    quotedTextT,
    nonindentSpaces,
    skipHorzSpace,
    wholelineP,
    endOfLineP,
  )
where

import qualified Data.Attoparsec.Text.Lazy as Atto
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as LTB

-- | Extract text between nested brackets.
--
-- The outer brackets are stripped off, but any inner brackets are retained.
--
-- FIXME: Should we deal with backslashes inside the brackets?
--
-- @since 0.5.0.0
matchingBracketP :: (Char, Char) -> Atto.Parser Text
matchingBracketP (opening, closing) =
  Atto.char opening
    *> go 0
    <* Atto.char closing
  where
    go :: Int -> Atto.Parser Text
    go n = do
      chars <- many $ Atto.satisfy (\c -> c /= opening && c /= closing)
      next <- Atto.peekChar'
      rest <-
        if next == opening
          then Atto.char opening *> ((one opening <>) <$> go (n + 1))
          else
            if next == closing && n > 0
              then Atto.char closing *> ((one closing <>) <$> go (n - 1))
              else pure mempty
      pure (toText chars <> rest)

-- | Extract the text between double or single quotes.  If there are
-- any backslash escaped quotes they will be extracted without the
-- backslash.
--
-- @since 0.5.0.0
quotedTextP :: Atto.Parser Text
quotedTextP = do
  char <- Atto.satisfy (\c -> c == '"' || c == '\'')
  go char <* Atto.char char
  where
    go :: Char -> Atto.Parser Text
    go quote = do
      chars <- many $ Atto.satisfy (\c -> c /= '\\' && c /= quote)
      next <- Atto.peekChar'
      if next == '\\'
        then (toText chars <>) <$> escaped quote
        else pure (toText chars)
    escaped :: Char -> Atto.Parser Text
    escaped quote = do
      _ <- Atto.char '\\'
      c <- Atto.anyChar
      if c == quote
        then (one quote <>) <$> go quote
        else (toText ['\\', c] <>) <$> go quote

-- | Render the given text inside quotes.  Any quotes in the text that
-- are not escape will be escaped with a backslash.
--
-- @since 0.5.0.0
quotedTextT :: Text -> LTB.Builder
quotedTextT =
  Text.foldl' (flip escape) (False, mempty)
    >>> snd
    >>> quote
  where
    escape :: Char -> (Bool, Text) -> (Bool, Text)
    escape c = \case
      (False, t)
        | c == '"' -> (False, t <> "\\\"")
        | otherwise -> (c == '\\', t <> one c)
      (True, t) -> (False, t <> one c)
    quote :: Text -> LTB.Builder
    quote t =
      mconcat
        [ LTB.singleton '"',
          LTB.fromText t,
          LTB.singleton '"'
        ]

-- | Allowed leading space in Markdown before it is considered an
-- indentation.  This parser never fails.
--
-- @since 0.5.0.0
nonindentSpaces :: Atto.Parser Int
nonindentSpaces = go 3 <|> pure 0
  where
    go :: Int -> Atto.Parser Int
    go n = do
      _ <- Atto.char ' '
      if n >= 0
        then (go (pred n) <&> (+ 1)) <|> pure 1
        else pure 1

-- | Skip horizontal space.
--
-- @since 0.5.0.0
skipHorzSpace :: Atto.Parser ()
skipHorzSpace = Atto.skipWhile Atto.isHorizontalSpace

-- | A single line of text.
wholelineP :: Atto.Parser Text
wholelineP = do
  chars <- many (Atto.satisfy (not . Atto.isEndOfLine))
  eol <- Atto.peekChar'
  Atto.endOfLine <|> Atto.endOfInput
  pure (toText chars <> if eol == '\r' then "\r" else "" <> "\n")

-- | Optional horizontal space followed by an end-of-line sequence.
--
-- @since 0.5.0.0
endOfLineP :: Atto.Parser Text
endOfLineP = (Atto.<?> "end of line") $ do
  space <- many (Atto.satisfy Atto.isHorizontalSpace)
  end <- newline <|> crnl <|> (Atto.endOfInput $> "\n")
  pure (toText space <> end)
  where
    newline :: Atto.Parser Text
    newline = Atto.char '\n' $> "\n"

    crnl :: Atto.Parser Text
    crnl = do
      _ <- Atto.char '\r'
      _ <- Atto.char '\n'
      pure "\r\n"
