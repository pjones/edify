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
module Edify.Markdown.AST
  ( AST,
    Block (..),
    InlineF (..),
    Inline,
    markdownP,
    markdownT,
    extractURLs,
  )
where

import qualified Data.Attoparsec.Text as Atto
import Data.Functor.Foldable (Fix (..), cata)
import qualified Data.Text.Lazy.Builder as LTB
import Edify.JSON
import Edify.Markdown.Common (endOfLineP, wholelineP)
import Edify.Markdown.Fence (FenceR)
import qualified Edify.Markdown.Fence as Fence
import Edify.Markdown.Heading (Heading)
import qualified Edify.Markdown.Heading as Heading
import Edify.Markdown.Image (Image)
import qualified Edify.Markdown.Image as Image
import Edify.Markdown.Link (Link)
import qualified Edify.Markdown.Link as Link

-- | Inline elements in Markdown.
--
-- @since 0.5.0.0
data InlineF r
  = TextChunkF Text
  | ImageF Image
  | LinkF (Link [r])
  deriving stock (Generic, Show, Eq, Functor)
  deriving (ToJSON, FromJSON) via GenericJSON (InlineF r)

-- | Recursive inline element.
--
-- @since 0.5.0.0
type Inline = Fix InlineF

deriving via (RecursiveJSON Inline) instance ToJSON Inline

deriving via (RecursiveJSON Inline) instance FromJSON Inline

-- | Block elements in Markdown.
--
-- @since 0.5.0.0
data Block
  = HeadingBlock Heading
  | FenceBlock (FenceR [Inline])
  | LinkDefBlock Link.Definition
  | ParaBlock [Inline]
  | BlankLine Text
  deriving stock (Generic)
  deriving (ToJSON, FromJSON) via GenericJSON Block

-- | Markdown syntax tree.  It's not really a tree, but more like a
-- list of trees.
--
-- @since 0.5.0.0
newtype AST = AST {unAST :: [Block]}

-- | Markdown parser.
--
-- @since 0.5.0.0
markdownP :: Atto.Parser AST
markdownP = AST <$> Atto.many1 blockP

-- | Parser a Markdown block.
--
-- @since 0.5.0.0
blockP :: Atto.Parser Block
blockP =
  Atto.peekChar >>= \case
    Nothing -> empty
    Just c
      | c == '\\' ->
        -- Disabled block, so parse it as a paragraph:
        ParaBlock <$> inlineP
      | otherwise ->
        Atto.choice [blankP, headingP, fenceP, linkDefP, paraP]
  where
    blankP :: Atto.Parser Block
    blankP = BlankLine <$> endOfLineP

    headingP :: Atto.Parser Block
    headingP = HeadingBlock <$> Heading.headingP

    fenceP :: Atto.Parser Block
    fenceP =
      FenceBlock
        . Fence.reinterpretFenceBodyP reinterpretInlineP
        <$> Fence.fenceP wholelineP

    linkDefP :: Atto.Parser Block
    linkDefP = LinkDefBlock <$> Link.linkDefinitionP

    paraP :: Atto.Parser Block
    paraP = ParaBlock <$> inlineP

-- | Parse as many Markdown inline elements as possible without
-- consuming a succeeding Markdown block.
--
-- @since 0.5.0.0
inlineP :: Atto.Parser [Inline]
inlineP = go
  where
    go :: Atto.Parser [Inline]
    go =
      Atto.peekChar >>= \case
        Nothing -> pure mempty
        Just c
          | c == '\\' -> (:) <$> textChunkP <*> go
          | c == '\r' || c == '\n' -> maybeEndOfPara
          | otherwise -> (:) <$> (imageP <|> linkP <|> textChunkP) <*> go

    -- Check for an end-of-paragraph condition.
    maybeEndOfPara :: Atto.Parser [Inline]
    maybeEndOfPara = do
      e0 <- endOfLineP
      e1 <- optional endOfLineP

      let eol = Fix (TextChunkF e0)
      if Just e0 == e1
        then pure [eol, eol]
        else (eol :) <$> go

    -- Image references.
    imageP :: Atto.Parser Inline
    imageP = Fix . ImageF <$> Image.imageP

    -- Links where the link text is parsed as inline markdown.
    linkP :: Atto.Parser Inline
    linkP =
      Link.linkP <&> Link.reinterpretLinkTextP reinterpretInlineP >>= \case
        Left msg -> fail ("parsing link text: " <> msg)
        Right link -> pure (Fix (LinkF link))

    -- Consume as many characters as possible without crossing a line
    -- boundary or consuming other syntax characters.
    --
    -- Always blindly consumes the first character.  Therefore this
    -- parser must come last.
    textChunkP :: Atto.Parser Inline
    textChunkP =
      fmap (Fix . TextChunkF . toText) $ do
        c <- Atto.anyChar
        cs <-
          many $
            Atto.satisfy
              ( \c ->
                  c /= '\\'
                    && c /= '!'
                    && c /= '['
                    && c /= '\n'
                    && c /= '\r'
              )
        pure (c : cs)

-- | A variant of 'inlineP' that consumes all input.
--
-- Use to reinterpret the body of a block.
--
-- @since 0.5.0.0
reinterpretInlineP :: Atto.Parser [Inline]
reinterpretInlineP =
  (<>)
    <$> inlineP
    <*> (one . Fix . TextChunkF . toText <$> many Atto.anyChar)

-- | Convert a Markdown syntax tree into text.
--
-- @since 0.5.0.0
markdownT :: AST -> LTB.Builder
markdownT = unAST >>> foldMap go
  where
    go :: Block -> LTB.Builder
    go = \case
      HeadingBlock h -> Heading.headingT h
      FenceBlock f -> Fence.fenceT (foldMap inlineT) f
      LinkDefBlock def -> Link.linkDefinitionT def
      ParaBlock ins -> foldMap inlineT ins
      BlankLine t -> LTB.fromText t

    inlineT :: Inline -> LTB.Builder
    inlineT = cata $ \case
      TextChunkF t -> LTB.fromText t
      ImageF img -> Image.imageT img
      LinkF lnk -> Link.linkT mconcat lnk

-- | Extract all URLs from links and images.
--
-- @since 0.5.0.0
extractURLs :: AST -> [Text]
extractURLs = unAST >>> concatMap go
  where
    go :: Block -> [Text]
    go = \case
      HeadingBlock {} -> mempty
      FenceBlock fb -> concatMap inline (Fence.extractBody fb & concat)
      LinkDefBlock def -> [Link.defURL def]
      ParaBlock pb -> concatMap inline pb
      BlankLine {} -> mempty

    inline :: Inline -> [Text]
    inline = cata $ \case
      TextChunkF {} ->
        mempty
      ImageF img ->
        case Image.imageSrc img of
          Link.Inline url _ -> pure url
          Link.Reference {} -> mempty
      LinkF lnk ->
        concat (Link.linkText lnk)
          <> case Link.linkDest lnk of
            Link.Inline url _ -> pure url
            Link.Reference {} -> mempty
