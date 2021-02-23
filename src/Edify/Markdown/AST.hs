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
    unAST,
    Block (..),
    InlineF (..),
    Inline,
    markdownP,
    markdownT,
    blocks,
    urls,
    fences,
    fencesRewrite,
  )
where

import qualified Data.Attoparsec.Text as Atto
import Data.Functor.Foldable (Fix (..), cata)
import Data.Generics.Labels ()
import qualified Data.Text.Lazy.Builder as LTB
import Edify.Markdown.Attributes (Attributes)
import qualified Edify.Markdown.Comment as Comment
import Edify.Markdown.Common (endOfLineP, wholelineP)
import Edify.Markdown.Fence (Fence)
import qualified Edify.Markdown.Fence as Fence
import Edify.Markdown.Heading (Heading)
import qualified Edify.Markdown.Heading as Heading
import Edify.Markdown.Image (Image)
import qualified Edify.Markdown.Image as Image
import Edify.Markdown.Include (Include)
import qualified Edify.Markdown.Include as Include
import Edify.Markdown.Link (Link)
import qualified Edify.Markdown.Link as Link
import qualified Edify.Text.Indent as Indent
import Edify.Text.JSON

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
  | FenceBlock (Fence [Inline])
  | LinkDefBlock Link.Definition
  | CommentBlock Comment.Comment
  | IncludeBlock Include
  | ParaBlock [Inline]
  | BlankLine Text
  | EmptyBlock
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
        Atto.choice
          [ blankP,
            headingP,
            fenceP,
            linkDefP,
            commentP,
            includeP,
            paraP
          ]
  where
    blankP :: Atto.Parser Block
    blankP = BlankLine <$> endOfLineP

    headingP :: Atto.Parser Block
    headingP = HeadingBlock <$> Heading.headingP

    fenceP :: Atto.Parser Block
    fenceP = do
      fence <- Fence.fenceP wholelineP
      case Fence.reinterpret reinterpretInlineP fence of
        Left msg -> fail msg
        Right x -> pure (FenceBlock x)

    linkDefP :: Atto.Parser Block
    linkDefP = LinkDefBlock <$> Link.linkDefinitionP

    commentP :: Atto.Parser Block
    commentP = CommentBlock <$> Comment.commentP

    includeP :: Atto.Parser Block
    includeP = IncludeBlock <$> Include.includeP

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
-- FIXME: If a paragraph starts with a syntax character (i.e. @#@)
-- then escape it with a backslash.
--
-- @since 0.5.0.0
markdownT :: AST -> LTB.Builder
markdownT = unAST >>> foldMap go
  where
    go :: Block -> LTB.Builder
    go = \case
      HeadingBlock h -> Heading.headingT h
      FenceBlock f -> Fence.fenceT inlineT f
      LinkDefBlock def -> Link.linkDefinitionT def
      CommentBlock cmt -> Comment.commentT cmt
      IncludeBlock inc -> Include.includeT inc
      ParaBlock ins -> inlineT ins
      BlankLine t -> LTB.fromText t
      EmptyBlock -> mempty

-- | Convert a list of 'Inline' values into text.
--
-- @since 0.5.0.0
inlineT :: [Inline] -> LTB.Builder
inlineT = foldMap $
  cata $ \case
    TextChunkF t -> LTB.fromText t
    ImageF img -> Image.imageT img
    LinkF lnk -> Link.linkT mconcat lnk

-- | Traversal for all blocks in a Markdown AST.
--
-- @since 0.5.0.0
blocks ::
  Applicative f =>
  (Block -> f [Block]) ->
  AST ->
  f AST
blocks f (AST blocks) = AST . concat <$> traverse f blocks

-- | Traversal for all URLs in a block of Markdown.
--
-- @since 0.5.0.0
urls ::
  forall f.
  Applicative f =>
  (Text -> f Text) ->
  Block ->
  f [Block]
urls f =
  fmap one . \case
    FenceBlock fb -> FenceBlock <$> Fence.bodies (traverse inline) fb
    LinkDefBlock def -> LinkDefBlock <$> #defURL f def
    ParaBlock pb -> ParaBlock <$> traverse inline pb
    other -> pure other
  where
    inline :: Inline -> f Inline
    inline = cata $ \case
      TextChunkF t -> pure (Fix $ TextChunkF t)
      ImageF image -> Fix . ImageF <$> Image.src f image
      LinkF lnk -> Fix . LinkF <$> Link.traverseLink f sequenceA lnk

-- | Traversal for all fence blocks in a block of Markdown.
--
-- @since 0.5.0.0
fences ::
  forall f.
  Applicative f =>
  (Fence [Inline] -> f (Fence [Inline])) ->
  Block ->
  f Block
fences f = \case
  FenceBlock fb -> FenceBlock <$> f fb
  other -> pure other

-- | Traversal for rewriting fence blocks a la 'Fence.rewrite'.
--
-- @since 0.5.0.0
fencesRewrite ::
  forall f.
  Monad f =>
  Indent.Tabstop ->
  ((Attributes, Text) -> f Fence.Rewrite) ->
  Block ->
  f (Either Fence.RewriteError Block)
fencesRewrite tabstop f = \case
  FenceBlock fb ->
    Fence.rewrite tabstop inlineT inlineP f fb
      <&> second (maybe EmptyBlock FenceBlock)
  other ->
    pure (Right other)
