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
module Edify.Markdown.Heading
  ( Heading (..),
    headingP,
    headingT,
  )
where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as LTB
import Edify.JSON
import Edify.Markdown.Attributes
import Edify.Markdown.Common (endOfLineP, nonindentSpaces)

-- | Information about a Markdown heading.
--
-- @since 0.5.0.0
data Heading = Heading
  { headingLevel :: Int,
    headingContent :: Text,
    headingAttrs :: Maybe Attributes
  }
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via GenericJSON Heading

-- | Heading parser.
--
-- @since 0.5.0.0
headingP :: Atto.Parser Heading
headingP = atxHeadingP <|> setextHeadingP

-- | Help parse ATX headings.
data ATX
  = AtxContent Text
  | AtxClose
  | AtxAttrs Attributes

-- | Parse an ATX (structured text format from Aaron Swartz) style
-- heading.
--
-- @since 0.5.0.0
atxHeadingP :: Atto.Parser Heading
atxHeadingP = (Atto.<?> "ATX-style heading") $ do
  _ <- nonindentSpaces
  level <- Atto.many1 (Atto.char '#' $> Sum 1) <&> (fold >>> getSum)
  _ <- Atto.satisfy Atto.isHorizontalSpace
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
  _ <- endOfLineP
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
setextHeadingP :: Atto.Parser Heading
setextHeadingP = (Atto.<?> "Setext-style heading") $ do
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
  _ <- endOfLineP
  indent1 <- nonindentSpaces
  guard (indent0 == indent1)
  level <-
    (Atto.many1 (Atto.char '=') $> 1)
      <|> (Atto.many1 (Atto.char '-') $> 2)
  _ <- endOfLineP
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
