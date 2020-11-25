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
  ( Fence (..),
    FenceR,
    Style (..),
    fenceP,
    fenceT,
    reinterpretFenceBodyP,
    extractBody,
  )
where

import qualified Data.Attoparsec.Text as Atto
import Data.Functor.Foldable (Fix (..), cata)
import qualified Data.Text.Lazy.Builder as LTB
import Edify.JSON
import qualified Edify.Markdown.Attributes as Attrs
import Edify.Markdown.Common (endOfLineP, skipHorzSpace, wholelineP)

-- | Identifies the type of a fenced block.
--
-- @since 0.5.0.0
data Style
  = -- | Alternate form of an HTML @<div>@ element.
    Div
  | -- | Fenced code block.  Since multiple styles exist the character
    -- used by the author is recorded so the generated markdown looks
    -- the same as the parsed markdown.
    Code Char
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via GenericJSON Style

-- | A fenced block.
--
-- @since 0.5.0.0
data Fence r = Fence
  { -- | The type of block.
    fenceStyle :: Style,
    -- | How many spaces were used to indent the block.
    fenceIndent :: Int,
    -- | How many fence characters were used.
    fenceCount :: Int,
    -- | The line ending used after the fence characters.
    fenceLineEnd :: Text,
    -- | Fence attributes (classes, IDs, etc.).
    fenceAttrs :: Attrs.Attributes,
    -- | The body of the fenced block which is a list of type @r@.
    -- This is a recursive structure since some fence blocks can
    -- contain other fence blocks, or more markdown.
    fenceBody :: [r]
  }
  deriving stock (Generic, Show, Eq, Functor)
  deriving (ToJSON, FromJSON) via GenericJSON (Fence r)

-- | The (recursive) body of a fence block.
--
-- @since 0.5.0.0
data FenceBody t r
  = VerbatimBody Text
  | MarkdownBody t
  | NestedFence r
  deriving stock (Generic, Show, Eq, Functor)
  deriving (ToJSON, FromJSON) via GenericJSON (FenceBody t r)

-- | A variant of 'Fence' that clarifies the body type, with recursion
-- abstracted for use with @recursion-schemes@.  See 'FenceR'.
--
-- @since 0.5.0.0
newtype FenceF t r = FenceF (Fence (FenceBody t r))
  deriving stock (Generic, Show, Eq, Functor)
  deriving (ToJSON, FromJSON) via GenericJSON (FenceF t r)

-- | Fully recursive version of 'FenceF' for use with
-- @recursion-schemes@.
--
-- @since 0.5.0.0
type FenceR t = Fix (FenceF t)

deriving via (RecursiveJSON (FenceR t)) instance ToJSON t => ToJSON (FenceR t)

deriving via (RecursiveJSON (FenceR t)) instance FromJSON t => FromJSON (FenceR t)

-- | Parser for a recursive fence block.
--
-- The given parser should not consume more than a single line of
-- input.  Consuming more than a single line of input may result in
-- this parser failing to find the closing fence.
--
-- To create a @FenceR Text@ use 'wholelineP'.
--
-- @since 0.5.0.0
fenceP :: Semigroup r => Atto.Parser r -> Atto.Parser (FenceR r)
fenceP p = (divP p <|> codeP) <&> concatAdjacentText

-- | Div blocks.
divP :: Atto.Parser r -> Atto.Parser (FenceR r)
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
          [ NestedFence <$> divP parser,
            NestedFence <$> codeP,
            MarkdownBody <$> parser
          ]
      )
      (closingP ':' indent Nothing)
  pure $
    Fix $
      FenceF $
        Fence
          { fenceStyle = Div,
            fenceIndent = indent,
            fenceCount = colons,
            fenceLineEnd = lineEnd,
            fenceAttrs = attrs,
            fenceBody = body
          }

-- | Fenced code blocks.
codeP :: Atto.Parser (FenceR r)
codeP = (Atto.<?> "fenced code block") $ do
  indent <- many (Atto.satisfy Atto.isHorizontalSpace) <&> length
  c <- Atto.peekChar'
  if c == '`' || c == '~'
    then go indent c
    else empty Atto.<?> "expecting fenced character ` or ~"
  where
    go :: Int -> Char -> Atto.Parser (FenceR r)
    go indent char = do
      chars <- atLeastThreeP char <* skipHorzSpace
      attrs <- barewordP <|> Attrs.attributesP <|> pure mempty
      lineEnd <- endOfLineP
      body <-
        Atto.manyTill
          (VerbatimBody <$> wholelineP)
          (closingP char indent (Just chars))
      pure $
        Fix $
          FenceF $
            Fence
              { fenceStyle = Code char,
                fenceIndent = indent,
                fenceCount = chars,
                fenceLineEnd = lineEnd,
                fenceAttrs = attrs,
                fenceBody = body
              }

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
  css <- Attrs.cssIdentifierP
  pure $ Attrs.Attributes Nothing (one css) mempty

-- | Convert a 'FenceR' value into a lazy text builder.
--
-- @since 0.5.0.0
fenceT :: forall t. (t -> LTB.Builder) -> FenceR t -> LTB.Builder
fenceT encoder = cata go
  where
    go :: FenceF t LTB.Builder -> LTB.Builder
    go (FenceF Fence {..}) =
      let char = case fenceStyle of
            Div -> ':'
            Code c -> c
          attrs = case fenceStyle of
            Div ->
              LTB.singleton ' '
                <> Attrs.attributesShortcutT id id id fenceAttrs
            Code _ ->
              Attrs.attributesShortcutT
                id
                (const mempty)
                (LTB.singleton ' ' <>)
                fenceAttrs
          body = \case
            VerbatimBody t -> LTB.fromText t
            MarkdownBody t -> encoder t
            NestedFence b -> b
       in mconcat
            [ replicate fenceIndent (LTB.singleton ' ') & mconcat,
              replicate fenceCount (LTB.singleton char) & mconcat,
              attrs,
              LTB.fromText fenceLineEnd,
              foldMap body fenceBody,
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
-- [MarkdownBody "foo", MarkdownBody "bar", NestedFence, MarkdownBody "baz"] ->
-- [MarkdownBody "foobar", NestedFence, MarkdownBody "baz"]
-- @
--
-- @since 0.5.0.0
concatAdjacentText :: forall t. Semigroup t => FenceR t -> FenceR t
concatAdjacentText = cata go
  where
    go :: FenceF t (FenceR t) -> FenceR t
    go (FenceF f@Fence {}) =
      Fix (FenceF f {fenceBody = body (fenceBody f)})

    -- Process the body of a 'Fence' value.
    body :: [FenceBody t (FenceR t)] -> [FenceBody t (FenceR t)]
    body =
      foldr accum (Nothing, []) >>> \case
        (Just t, fs) -> MarkdownBody t : fs
        (Nothing, fs) -> fs

    -- Accumulate lines of markdown into the largest group possible.
    accum ::
      FenceBody t (FenceR t) ->
      (Maybe t, [FenceBody t (FenceR t)]) ->
      (Maybe t, [FenceBody t (FenceR t)])
    accum f (text, fs) = case f of
      MarkdownBody t -> (Just t <> text, fs)
      VerbatimBody _ -> case text of
        Nothing -> (Nothing, f : fs)
        Just t -> (Nothing, MarkdownBody t : f : fs)
      NestedFence _ -> case text of
        Nothing -> (Nothing, f : fs)
        Just t -> (Nothing, f : MarkdownBody t : fs)

-- | Reinterpret the body of a fence using the given parser.
--
-- The supplied parser must consume all input.  If the parser fails
-- the body text will be moved into a 'VerbatimBody' block.
--
-- @since 0.5.0.0
reinterpretFenceBodyP ::
  forall r.
  -- | A parser that will consume the entire body of a fenced block.
  Atto.Parser r ->
  -- | The fence structure to process.
  FenceR Text ->
  -- | The reinterpreted fence block.
  FenceR r
reinterpretFenceBodyP parser = cata go
  where
    go :: FenceF Text (FenceR r) -> FenceR r
    go (FenceF fence) =
      Fix (FenceF fence {fenceBody = map update (fenceBody fence)})
    update :: FenceBody Text (FenceR r) -> FenceBody r (FenceR r)
    update = \case
      MarkdownBody t -> runParser t
      VerbatimBody t -> VerbatimBody t
      NestedFence n -> NestedFence n
    runParser :: Text -> FenceBody r (FenceR r)
    runParser input =
      case Atto.parseOnly (parser <* Atto.endOfInput) input of
        Left _ -> VerbatimBody input
        Right r -> MarkdownBody r

-- | Extract the custom body type out of a fence.
--
-- @since 0.5.0.0
extractBody :: forall t. FenceR t -> [t]
extractBody = cata go
  where
    go :: FenceF t [t] -> [t]
    go (FenceF Fence {..}) = concat (mapMaybe extract fenceBody)

    extract :: FenceBody t [t] -> Maybe [t]
    extract = \case
      MarkdownBody t -> Just [t]
      VerbatimBody {} -> Nothing
      NestedFence ts -> Just ts
