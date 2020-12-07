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
-- Parse and generate markdown fenced blocks.
module Edify.Markdown.Fence
  ( Fence,
    Props (..),
    fenceP,
    fenceT,
    bodies,
    attrs,
    reinterpret,

    -- * Rewriting Fences
    Rewrite (..),
    Rewritten,
    rewrite,
  )
where

import Control.Lens ((.~))
import qualified Data.Attoparsec.Text as Atto
import Data.Functor.Foldable (Fix (..), cata, embed, project)
import Data.Generics.Labels ()
import qualified Data.Text.Lazy.Builder as LTB
import Edify.JSON
import Edify.Markdown.Attributes (Attributes)
import qualified Edify.Markdown.Attributes as Attrs
import Edify.Markdown.Common (endOfLineP, skipHorzSpace, wholelineP)

-- | Fence block proprieties.
--
-- @since 0.5.0.0
data Props = Props
  { -- | How many spaces were used to indent the block.
    fenceIndent :: Int,
    -- | How many fence characters were used.
    fenceCount :: Int,
    -- | The line ending used after the fence characters.
    fenceLineEnd :: Text,
    -- | Fence attributes (classes, IDs, etc.).
    fenceAttrs :: Attrs.Attributes
  }
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via GenericJSON Props

-- | An implicit recursive fence structure.
--
-- @since 0.5.0.0
data FenceF t r
  = -- | The body of a fence.  Since this constructor is private it is
    -- never the root of the tree.
    FenceBody t
  | -- | Code fences.  These are always leafs since their bodies
    -- cannot contain more markdown.
    CodeFence Char Props Text
  | -- | Div fences.  Their bodies are made up of the other
    -- constructors.
    DivFence Props [r]
  deriving stock (Generic, Show, Eq, Functor)
  deriving (ToJSON, FromJSON) via GenericJSON (FenceF t r)

-- | An explicitly recursive fence structure.
--
-- @since 0.5.0.0
type Fence t = Fix (FenceF t)

deriving via (RecursiveJSON (Fence t)) instance ToJSON t => ToJSON (Fence t)

deriving via (RecursiveJSON (Fence t)) instance FromJSON t => FromJSON (Fence t)

-- | Parser for a recursive fence block.
--
-- The given parser should not consume more than a single line of
-- input.  Consuming more than a single line of input may result in
-- this parser failing to find the closing fence.
--
-- To create a @FenceR Text@ use 'wholelineP'.
--
-- @since 0.5.0.0
fenceP :: Semigroup t => Atto.Parser t -> Atto.Parser (Fence t)
fenceP p = (divP p <|> codeP) <&> concatAdjacentText

-- | Div blocks.
divP :: Atto.Parser t -> Atto.Parser (Fence t)
divP parser = (Atto.<?> "fenced div block") $ do
  indent <- many (Atto.satisfy Atto.isHorizontalSpace) <&> length
  colons <- atLeastThreeP ':' <* skipHorzSpace
  attrs <- barewordP <|> Attrs.attributesP
  lineEnd <-
    skipHorzSpace
      *> Atto.skipWhile (== ':')
      *> endOfLineP
  body <-
    Atto.manyTill
      ( Atto.choice
          [ divP parser,
            codeP,
            embed . FenceBody <$> parser
          ]
      )
      (closingP ':' indent Nothing)
  let props =
        Props
          { fenceIndent = indent,
            fenceCount = colons,
            fenceLineEnd = lineEnd,
            fenceAttrs = attrs
          }
  pure $ embed (DivFence props body)

-- | Fenced code blocks.
codeP :: Atto.Parser (Fence t)
codeP = (Atto.<?> "fenced code block") $ do
  indent <- many (Atto.satisfy Atto.isHorizontalSpace) <&> length
  c <- Atto.peekChar'
  if c == '`' || c == '~'
    then go indent c
    else empty Atto.<?> "expecting fenced character ` or ~"
  where
    go :: Int -> Char -> Atto.Parser (Fence t)
    go indent char = do
      chars <- atLeastThreeP char <* skipHorzSpace
      attrs <- barewordP <|> Attrs.attributesP <|> pure mempty
      lineEnd <- endOfLineP
      body <- Atto.manyTill wholelineP (closingP char indent (Just chars))
      let props =
            Props
              { fenceIndent = indent,
                fenceCount = chars,
                fenceLineEnd = lineEnd,
                fenceAttrs = attrs
              }
      pure $ embed (CodeFence char props (mconcat body))

-- | The closing of a fence block.
closingP ::
  -- | The character used to mark the fence.
  Char ->
  -- | The indentation level.
  Int ->
  -- | Whether the fence character needs to exist exactly N times
  -- or any count >= 3.
  Maybe Int ->
  -- | The parser that returns unit.
  Atto.Parser ()
closingP char indent exactly =
  Atto.count indent (Atto.satisfy Atto.isHorizontalSpace)
    *> maybe
      (atLeastThreeP char $> ())
      (\n -> Atto.count n (Atto.char char) $> ())
      exactly
    *> (endOfLineP $> ())

-- | Read /N/ copies of 'Char', then any remaining instances.
atLeastThreeP :: Char -> Atto.Parser Int
atLeastThreeP c = (Atto.<?> ("at least three " <> one c)) $ do
  _ <- Atto.count 3 (Atto.char c)
  cs <- many (Atto.char c)
  pure (length cs + 3)

-- | Shortcut for an attribute list with a single class.
--
-- Example:
--
-- @
--   ::: Foo
--   :::
--
--   ```haskell
--   ```
-- @
barewordP :: Atto.Parser Attrs.Attributes
barewordP = (Atto.<?> "fenced block class name shortcut") $ do
  char <- Atto.peekChar'
  guard (char /= '{')
  css <- Attrs.cssIdentP
  pure $ Attrs.Attributes Nothing (one css) mempty

-- | Convert a 'FenceR' value into a lazy text builder.
--
-- @since 0.5.0.0
fenceT :: forall t. (t -> LTB.Builder) -> Fence t -> LTB.Builder
fenceT encoder = cata go
  where
    go :: FenceF t LTB.Builder -> LTB.Builder
    go = \case
      FenceBody b -> encoder b
      CodeFence char props@Props {fenceAttrs} body ->
        let spc = (LTB.singleton ' ' <>)
            attrs = Attrs.attributesShortcutT id (const mempty) spc fenceAttrs
         in format char props attrs (LTB.fromText body)
      DivFence props@Props {fenceAttrs} body ->
        let attrs = Attrs.attributesShortcutT id id id fenceAttrs
         in format ':' props (LTB.singleton ' ' <> attrs) (mconcat body)

    format ::
      -- | Fence character.
      Char ->
      -- | Fence properties.
      Props ->
      -- | Encoded attributes.
      LTB.Builder ->
      -- | Encoded body.
      LTB.Builder ->
      -- | Formatted fence.
      LTB.Builder
    format char Props {..} attrs body =
      mconcat
        [ replicate fenceIndent (LTB.singleton ' ') & mconcat,
          replicate fenceCount (LTB.singleton char) & mconcat,
          attrs,
          LTB.fromText fenceLineEnd,
          body,
          replicate fenceIndent (LTB.singleton ' ') & mconcat,
          replicate fenceCount (LTB.singleton char) & mconcat,
          LTB.fromText fenceLineEnd
        ]

-- | Process the body of each fenced block, concatenating adjacent
-- lines of text into a single text value.
--
-- Example:
--
-- @
-- [FenceBody "foo", FenceBody "bar", CodeFence, FenceBody "baz"] ->
-- [FenceBody "foobar", CodeFence, FenceBody "baz"]
-- @
--
-- @since 0.5.0.0
concatAdjacentText :: forall t. Semigroup t => Fence t -> Fence t
concatAdjacentText = cata go
  where
    go :: FenceF t (Fence t) -> Fence t
    go =
      embed . \case
        fence@FenceBody {} -> fence
        fence@CodeFence {} -> fence
        DivFence props body ->
          DivFence props $
            map embed $
              case foldr (accum . project) (Nothing, []) body of
                (Just t, fs) -> FenceBody t : fs
                (Nothing, fs) -> fs

    -- Accumulate lines of markdown into the largest group possible.
    accum ::
      FenceF t (Fence t) ->
      (Maybe t, [FenceF t (Fence t)]) ->
      (Maybe t, [FenceF t (Fence t)])
    accum f (prev, fs) =
      case f of
        FenceBody b -> (Just b <> prev, fs)
        _others -> case prev of
          Nothing -> (Nothing, f : fs)
          Just b -> (Nothing, f : FenceBody b : fs)

-- | Reinterpret the body of a fence using the given parser.
--
-- The supplied parser must consume all input.
--
-- @since 0.5.0.0
reinterpret ::
  forall r.
  -- | A parser that will consume the entire body of a fenced block.
  Atto.Parser r ->
  -- | The fence structure to process.
  Fence Text ->
  -- | The reinterpreted fence block.
  Either String (Fence r)
reinterpret parser =
  bodies (Atto.parseOnly (parser <* Atto.endOfInput))

-- | A traversal over the bodies that are of type @a@.
--
-- @since 0.5.0.0
bodies ::
  forall a b f.
  Applicative f =>
  (a -> f b) ->
  Fence a ->
  f (Fence b)
bodies f = cata go
  where
    go :: FenceF a (f (Fence b)) -> f (Fence b)
    go =
      fmap embed . \case
        FenceBody b -> FenceBody <$> f b
        CodeFence c p b -> pure (CodeFence c p b)
        DivFence p b -> DivFence p <$> sequenceA b

-- | A traversal over the attributes for each fence in the recursive
-- fence structure.
--
-- @since 0.5.0.0
attrs ::
  forall f t.
  Applicative f =>
  -- | The traversing function.
  (Attributes -> f Attributes) ->
  -- | The fence structure to traverse.
  Fence t ->
  -- | The result of the traversal.
  f (Fence t)
attrs f = cata go
  where
    go :: FenceF t (f (Fence t)) -> f (Fence t)
    go =
      fmap embed . \case
        FenceBody body ->
          pure (FenceBody body)
        CodeFence chr props body ->
          CodeFence chr
            <$> #fenceAttrs f props
            <*> pure body
        DivFence props body ->
          DivFence
            <$> #fenceAttrs f props
            <*> sequenceA body

-- | Instructs the 'rewrite' function how to rewrite portions of a
-- recursive fence structure.
--
-- @since 0.5.0.0
data Rewrite = Rewrite
  { -- | When 'Just', update fence attributes.
    rewriteAttrs :: Maybe Attrs.Attributes,
    -- | When 'Just', replace the fence's body.  If there are fences
    -- nested under this fence then they will *not* be preserved.
    rewriteBody :: Maybe Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via GenericJSON Rewrite

instance Semigroup Rewrite where
  (<>) (Rewrite x1 y1) (Rewrite x2 y2) =
    Rewrite (x1 <|> x2) (y1 <|> y2)

instance Monoid Rewrite where
  mempty = Rewrite Nothing Nothing

-- | Helper type mostly to reduce keyboard typing.
--
-- @since 0.5.0.0
type Rewritten t = Either String (Fence t)

-- | A traversal that can rewrite portions of the recursive fence
-- structure.
--
-- @since 0.5.0.0
rewrite ::
  forall t f.
  Semigroup t =>
  Monad f =>
  -- | A function to convert value of type @t@ to a text builder.
  (t -> LTB.Builder) ->
  -- | Parser used to post-process a rewritten body.
  Atto.Parser t ->
  -- | The function that generates rewrites.
  ((Attributes, Text) -> f Rewrite) ->
  -- | The original fence structure.
  Fence t ->
  -- | The updated fence structure.
  f (Rewritten t)
rewrite encode parser f = cata go
  where
    go :: FenceF t (f (Rewritten t)) -> f (Rewritten t)
    go = \case
      FenceBody b -> pure (Right $ embed (FenceBody b))
      CodeFence char props body ->
        f (fenceAttrs props, body)
          <&> ( \Rewrite {..} ->
                  Right $
                    embed $
                      CodeFence
                        char
                        (attrs rewriteAttrs props)
                        $ fromMaybe body rewriteBody
              )
      DivFence props body ->
        sequenceA body <&> sequenceA
          >>= either
            (pure . Left)
            ( \bodies -> do
                let text =
                      foldMap (fenceT encode) bodies
                        & LTB.toLazyText
                        & toStrict
                Rewrite {..} <- f (fenceAttrs props, text)
                case rewriteBody of
                  Nothing ->
                    DivFence (attrs rewriteAttrs props) bodies
                      & embed
                      & Right
                      & pure
                  Just b -> pure (parse (attrs rewriteAttrs props) b)
            )

    -- Optionally update the attributes inside the given 'Props'.
    attrs :: Maybe Attributes -> Props -> Props
    attrs = \case
      Nothing -> id
      Just a -> #fenceAttrs .~ a

    -- Parse the body of a div fence and reconstruct it.
    parse :: Props -> Text -> Rewritten t
    parse props body =
      let p =
            Atto.many1 $
              Atto.choice
                [ divP parser,
                  codeP,
                  embed . FenceBody <$> parser
                ]
       in Atto.parseOnly (p <* Atto.endOfInput) body
            <&> (DivFence props >>> embed >>> concatAdjacentText)
