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
module Edify.Format.Markdown
  ( Error (..),
    Chunk (..),
    Heading (..),
    toChunks,
    fromChunks,
    narrow,
  )
where

import qualified Byline as B
import qualified Data.Attoparsec.Text.Lazy as Atto
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as LTB
import Edify.JSON
import Edify.Text.Attributes
import Edify.Text.Narrow (Token (..))

-- | Errors that may occur.
--
-- @since 0.5.0.0
data Error
  = ParseError String
  | NarrowTokenMissingError Text
  deriving (Generic, Show)

instance B.ToStylizedText Error where
  toStylizedText = \case
    ParseError err ->
      B.text "markdown parsing error: "
        <> (B.fg B.red <> fromString err)
    NarrowTokenMissingError t ->
      B.text "I didn't see a Markdown heading with ID "
        <> (B.fg B.red <> B.text t)

-- | Information about a Markdown heading.
--
-- @since 0.5.0.0
data Heading = Heading
  { headingLevel :: Int,
    headingContent :: Text,
    headingAttrs :: Maybe Attributes
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via GenericJSON Heading

-- | A single chunk of Markdown.
--
-- @since 0.5.0.0
data Chunk
  = Chunk Text
  | CHeading Heading
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via GenericJSON Chunk

-- | Parse the given input and produce a 'Chunk' list.
--
-- @since 0.5.0.0
toChunks :: LText -> Either Error [Chunk]
toChunks =
  Atto.parse parser >>> \case
    Atto.Done _ cs ->
      Right cs
    Atto.Fail _ [] msg ->
      Left (ParseError msg)
    Atto.Fail _ ctx msg ->
      (intercalate " > " ctx <> ": " <> msg)
        & ParseError
        & Left
  where
    parser :: Atto.Parser [Chunk]
    parser =
      Atto.many1
        ( Atto.choice
            [ atxHeading <&> CHeading,
              setextHeading <&> CHeading,
              chunk <&> Chunk
            ]
        )
        <* Atto.endOfInput
    chunk :: Atto.Parser Text
    chunk = do
      ts <- many (Atto.satisfy (Atto.isEndOfLine >>> not))
      nl <- Atto.many1 (Atto.satisfy Atto.isEndOfLine)
      pure (toText ts <> toText nl)

-- | Convert a 'Chunk' list back to 'LText'.
--
-- @since 0.5.0.0
fromChunks :: [Chunk] -> LText
fromChunks = foldMap go >>> LTB.toLazyText
  where
    go :: Chunk -> LTB.Builder
    go = \case
      Chunk t -> LTB.fromText t
      CHeading h -> headingT h

-- | Narrow Markdown so that it only includes the named heading and
-- its children.
--
-- @since 0.5.0.0
narrow :: Token -> LText -> Either Error LText
narrow (Token token) input = do
  cs <- toChunks input
  foldl' go (mempty, Nothing) cs & \case
    ([], _) -> Left (NarrowTokenMissingError token)
    (xs, _) -> Right (fromChunks $ reverse xs)
  where
    -- Narrow a list of chucks via a fold.
    go :: ([Chunk], Maybe Int) -> Chunk -> ([Chunk], Maybe Int)
    go (cs, level) c =
      case c of
        Chunk _ -> (maybe cs (const (c : cs)) level, level)
        CHeading Heading {..} ->
          case level of
            Nothing ->
              if Just token == ((headingAttrs >>= attrID) <&> getAttrName)
                then (c : cs, Just headingLevel)
                else (cs, Nothing)
            Just n ->
              if headingLevel > n
                then (c : cs, level)
                else (cs, Nothing)

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

-- | Matches the end of a line or the end of input.
--
-- @since 0.5.0.0
eolP :: Atto.Parser ()
eolP =
  (Atto.<?> "expected end of line or input") $
    Atto.skipWhile Atto.isHorizontalSpace
      *> (Atto.endOfLine <|> Atto.endOfInput)

-- | Help parse ATX headings.
data ATX
  = AtxContent Text
  | AtxClose
  | AtxAttrs Attributes

-- | Parse an ATX (structured text format from Aaron Swartz) style
-- heading.
--
-- @since 0.5.0.0
atxHeading :: Atto.Parser Heading
atxHeading = (Atto.<?> "ATX-style heading") $ do
  _ <- nonindentSpaces
  level <- Atto.many1 (Atto.char '#' $> Sum 1) <&> (fold >>> getSum)
  Atto.skipWhile Atto.isHorizontalSpace
  guard (level <= 6)
  let close =
        Atto.skipWhile Atto.isHorizontalSpace
          *> Atto.many1 (Atto.satisfy (== '#'))
          *> Atto.skipWhile Atto.isHorizontalSpace
          *> ( Atto.peekChar >>= \case
                 Nothing -> pure ()
                 Just c
                   | c == '{' -> pure ()
                   | c == '\r' -> pure ()
                   | c == '\n' -> pure ()
                   | otherwise -> empty
             )
      heading =
        Atto.many1
          ( Atto.satisfy
              ( \c ->
                  c /= '#'
                    && c /= '{'
                    && c /= '\r'
                    && c /= '\n'
              )
          )
  content <-
    Atto.many1 $
      Atto.choice
        [ heading <&> (toText >>> AtxContent),
          close $> AtxClose,
          attributesP <&> AtxAttrs,
          Atto.char '#' $> AtxContent "#",
          Atto.char '{' $> AtxContent "{"
        ]
  eolP
  pure $
    Heading
      { headingLevel = level,
        headingContent = Text.strip (mconcat [t | AtxContent t <- content]),
        headingAttrs = listToMaybe [a | AtxAttrs a <- content]
      }

-- | Help parse Setext headings.
data Setext
  = SetextContent Text
  | SetextAttrs Attributes

-- | Parse a Setext (Structure Enhanced Text) heading.
--
-- @since 0.5.0.0
setextHeading :: Atto.Parser Heading
setextHeading = (Atto.<?> "Setext-style heading") $ do
  indent0 <- nonindentSpaces
  let headingP =
        Atto.many1
          ( Atto.satisfy
              ( \c ->
                  c /= '{'
                    && c /= '\r'
                    && c /= '\n'
              )
          )
  content <-
    Atto.many1 $
      Atto.choice
        [ headingP <&> (toText >>> SetextContent),
          attributesP <&> SetextAttrs,
          Atto.char '{' $> SetextContent "{"
        ]
  eolP
  indent1 <- nonindentSpaces
  guard (indent0 == indent1)
  level <-
    (Atto.many1 (Atto.char '=') $> 1)
      <|> (Atto.many1 (Atto.char '-') $> 2)
  eolP
  pure $
    Heading
      { headingLevel = level,
        headingContent = Text.strip (mconcat [t | SetextContent t <- content]),
        headingAttrs = listToMaybe [a | SetextAttrs a <- content]
      }

-- | Encode a 'Heading'.
--
-- @since 0.5.0.0
headingT :: Heading -> LTB.Builder
headingT Heading {..} =
  mconcat
    [ LTB.fromText $ Text.replicate headingLevel "#",
      LTB.singleton ' ',
      LTB.fromText headingContent,
      maybe mempty (attributesT >>> (LTB.singleton ' ' <>)) headingAttrs,
      LTB.fromText "\n"
    ]
