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
module Edify.Markdown.Link
  ( Link (..),
    Definition (..),
    RefSep (..),
    Destination (..),
    traverseLink,
    linkP,
    linkDestP,
    linkDefinitionP,
    linkT,
    linkDestT,
    linkDefinitionT,
    reinterpretLinkTextP,
  )
where

import qualified Data.Attoparsec.Text.Lazy as Atto
import Data.Char (isSpace)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as LTB
import Edify.JSON
import Edify.Markdown.Common
  ( endOfLineP,
    matchingBracketP,
    quotedTextP,
    quotedTextT,
  )

-- | Records the space between a link's text and reference label.
--
-- @since 0.5.0.0
newtype RefSep = RefSep (Maybe Text)
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via GenericJSON RefSep

-- | Link destination.
--
-- @since 0.5.0.0
data Destination
  = -- | Links where the URL is given inline with an optional title.
    --
    -- Examples:
    --
    -- @
    -- [Link Text](url)
    -- [Link Text](https://eff.org "Electronic Frontier Foundation")
    -- @
    Inline Text (Maybe Text)
  | -- | Reference links where the URL needs to be looked up somewhere
    -- else in the document.  If no reference is given it is taken
    -- from the link text.
    --
    -- Examples:
    --
    -- @
    -- [Link Text][ref]
    -- [Link Text][]
    -- [Link Text]
    -- @
    Reference (Maybe Text) RefSep
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via GenericJSON Destination

-- | A link in Markdown.
--
-- The highlighted link text can contain Markdown, including images.
--
-- @since 0.5.0.0
data Link r
  = Link
      { -- | The text to be highlighted as a link.
        linkText :: r,
        -- | The type of link used.
        linkDest :: Destination
      }
  | AutoLink Text
  deriving stock (Generic, Show, Eq, Functor)
  deriving (ToJSON, FromJSON) via GenericJSON (Link r)

-- | A link definition (for reference links).
--
-- @since 0.5.0.0
data Definition = Definition
  { defName :: Text,
    defURL :: Text,
    defTitle :: Maybe Text,
    defEOL :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via GenericJSON Definition

-- | A @bitraverse@ over the URL and link text of a link.
--
-- @since 0.5.0.0
traverseLink ::
  Applicative f =>
  (Text -> f Text) ->
  (a -> f b) ->
  Link a ->
  f (Link b)
traverseLink f g = \case
  Link {..} ->
    case linkDest of
      Inline u t -> Link <$> g linkText <*> ((`Inline` t) <$> f u)
      Reference {} -> (`Link` linkDest) <$> g linkText
  AutoLink u -> AutoLink <$> f u

-- | Link parser.
--
-- @since 0.5.0.0
linkP :: Atto.Parser (Link Text)
linkP =
  Atto.choice
    [ Link <$> linkTextP <*> linkDestP,
      linkAutoP
    ]
    Atto.<?> "link"
  where
    linkTextP :: Atto.Parser Text
    linkTextP = matchingBracketP ('[', ']')

-- | Link destination parser.
--
-- @since 0.5.0.0
linkDestP :: Atto.Parser Destination
linkDestP = linkInlineP <|> linkRefP

-- | Inline destination parser.
--
-- @since 0.5.0.0
linkInlineP :: Atto.Parser Destination
linkInlineP = do
  _ <- Atto.char '('
  url <- Atto.many1 $ Atto.satisfy (\c -> not (isSpace c) && c /= ')')
  next <- Atto.peekChar'
  title <-
    if next == ')'
      then pure Nothing
      else optional titleP
  Atto.skipSpace <* Atto.char ')'
  pure (Inline (toText url) title)

-- | Reference destination parser.
--
-- @since 0.5.0.0
linkRefP :: Atto.Parser Destination
linkRefP = do
  -- Ensure this is not a link definition.
  c <- Atto.peekChar
  guard (c /= Just ':')

  sep <- RefSep <$> optional (spaceNewlineSpace <|> hSpace)
  optional (matchingBracketP ('[', ']'))
    >>= \case
      Nothing -> pure (Reference Nothing sep)
      Just ref
        | Text.all isSpace ref -> pure (Reference Nothing sep)
        | otherwise -> pure (Reference (Just $ Text.strip ref) sep)
  where
    spaceNewlineSpace :: Atto.Parser Text
    spaceNewlineSpace = do
      eol <- endOfLineP
      spc <- many $ Atto.satisfy Atto.isHorizontalSpace
      pure (eol <> toText spc)

    hSpace :: Atto.Parser Text
    hSpace = Atto.many1 (Atto.satisfy Atto.isHorizontalSpace) <&> toText

-- | Automatic links (Links between angle brackets).
--
-- @since 0.5.0.0
linkAutoP :: Atto.Parser (Link r)
linkAutoP = AutoLink <$> matchingBracketP ('<', '>')

-- | Link definition parser.
--
-- @since 0.5.0.0
linkDefinitionP :: Atto.Parser Definition
linkDefinitionP = do
  ref <- matchingBracketP ('[', ']') <* Atto.char ':'
  Atto.skipWhile Atto.isHorizontalSpace
  (url, title) <- definitionBodyP
  eol <- endOfLineP
  pure $
    Definition
      { defName = ref,
        defURL = url,
        defTitle = title,
        defEOL = eol
      }

-- | The body of a link definition.
--
-- Link definitions always contain a URL and can optionally be
-- followed by a link title.
--
-- @since 0.5.0.0
definitionBodyP :: Atto.Parser (Text, Maybe Text)
definitionBodyP = do
  url <- Atto.many1 (Atto.satisfy (not . isSpace)) <&> toText
  title <- optional titleP
  pure (url, title)

-- | Parse an optional link title.
--
-- @since 0.5.0.0
titleP :: Atto.Parser Text
titleP = do
  Atto.skipWhile Atto.isHorizontalSpace
  _ <-
    optional
      ( Atto.endOfLine
          *> Atto.many1 (Atto.satisfy Atto.isHorizontalSpace)
      )
  quotedTextP

-- | Render a link as text.
--
-- @since 0.5.0.0
linkT :: (r -> LTB.Builder) -> Link r -> LTB.Builder
linkT f = \case
  Link {..} ->
    mconcat
      [ LTB.singleton '[',
        f linkText,
        LTB.singleton ']',
        linkDestT linkDest
      ]
  AutoLink url ->
    mconcat
      [ LTB.singleton '<',
        LTB.fromText url,
        LTB.singleton '>'
      ]

-- | Render a link destination as text.
--
-- @since 0.5.0.0
linkDestT :: Destination -> LTB.Builder
linkDestT = \case
  Inline url title ->
    mconcat
      [ LTB.singleton '(',
        definitionBodyT url title,
        LTB.singleton ')'
      ]
  Reference ref (RefSep sep) ->
    mconcat
      [ maybe mempty LTB.fromText sep,
        LTB.singleton '[',
        maybe mempty LTB.fromText ref,
        LTB.singleton ']'
      ]

-- | Render the body of a link definition as text.
--
-- @since 0.5.0.0
definitionBodyT :: Text -> Maybe Text -> LTB.Builder
definitionBodyT url title =
  mconcat
    [ LTB.fromText url,
      maybe mempty ((LTB.singleton ' ' <>) . quotedTextT) title
    ]

-- | Render a link definition as text.
--
-- @since 0.5.0.0
linkDefinitionT :: Definition -> LTB.Builder
linkDefinitionT Definition {..} =
  mconcat
    [ LTB.singleton '[',
      LTB.fromText defName,
      LTB.fromText "]: ",
      definitionBodyT defURL defTitle,
      LTB.fromText defEOL
    ]

-- | Reinterpret the link text with another parser.
--
-- @since 0.5.0.0
reinterpretLinkTextP ::
  -- | A parser that must consume all of the link text.
  Atto.Parser r ->
  -- | The link to reinterpret.
  Link Text ->
  -- | The updated link.
  Either String (Link r)
reinterpretLinkTextP parser = \case
  Link {..} ->
    (`Link` linkDest)
      <$> Atto.parseOnly (parser <* Atto.endOfInput) linkText
  AutoLink t -> pure (AutoLink t)
