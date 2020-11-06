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
module Edify.Text.Fence
  ( Fence (..),
    Style (..),
    allowsRecursiveMarkdown,
    fenceP,
    fenceT,
  )
where

import qualified Data.Attoparsec.Text.Lazy as Atto
import Data.Functor.Foldable (Fix (..), cata)
import qualified Data.Text.Lazy.Builder as LTB
import Edify.JSON
import qualified Edify.Text.Attributes as Attrs

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

-- | A variant of 'Fence' that clarifies the body type, with recursion
-- abstracted for use with @recursion-schemes@.  See 'FenceR'.
--
-- @since 0.5.0.0
data FenceF r
  = FenceF (Fence r)
  | BodyF Text
  deriving stock (Generic, Show, Eq, Functor)
  deriving (ToJSON, FromJSON) via GenericJSON (FenceF r)

-- | Fully recursive version of 'FenceF' for use with
-- @recursion-schemes@.
--
-- @since 0.5.0.0
type FenceR = Fix FenceF

-- | Does the textual body of a fence block with the given style
-- contain Markdown that needs to be post-processed?
--
-- @since 0.5.0.0
allowsRecursiveMarkdown :: Style -> Bool
allowsRecursiveMarkdown = \case
  Div -> True
  Code _ -> False

-- | Parser for a recursive fence block.
--
-- @since 0.5.0.0
fenceP :: Atto.Parser FenceR
fenceP = (divP <|> codeP) <&> concatAdjacentText

-- | Div blocks.
divP :: Atto.Parser FenceR
divP = (Atto.<?> "fenced div block") $ do
  indent <- many (Atto.satisfy Atto.isHorizontalSpace) <&> length
  colons <- atLeastThreeP ':' <* skipHorzSpace
  attrs <- barewordP <|> Attrs.attributesP
  lineEnd <-
    skipHorzSpace
      *> Atto.skipWhile (== ':')
      *> endOfLineP
  body <-
    Atto.manyTill
      (divP <|> codeP <|> wholelineP)
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
codeP :: Atto.Parser FenceR
codeP = (Atto.<?> "fenced code block") $ do
  indent <- many (Atto.satisfy Atto.isHorizontalSpace) <&> length
  c <- Atto.peekChar'
  if c == '`' || c == '~'
    then go indent c
    else empty Atto.<?> "expecting fenced character ` or ~"
  where
    go :: Int -> Char -> Atto.Parser FenceR
    go indent char = do
      chars <- atLeastThreeP char <* skipHorzSpace
      attrs <- (barewordP <|> Attrs.attributesP <|> pure mempty)
      lineEnd <- endOfLineP
      body <- Atto.manyTill wholelineP (closingP char indent (Just chars))
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

-- | A single line of text.
wholelineP :: Atto.Parser FenceR
wholelineP = do
  chars <- many (Atto.satisfy (Atto.isEndOfLine >>> not))
  eol <- Atto.peekChar'
  Atto.endOfLine <|> Atto.endOfInput
  pure $
    Fix $
      BodyF
        ( toText chars
            <> if eol == '\n'
              then "\n"
              else "\r\n"
        )

-- | Skip horizontal space.
skipHorzSpace :: Atto.Parser ()
skipHorzSpace = Atto.skipWhile Atto.isHorizontalSpace

-- | Optional white-space followed by an end-of-line sequence.
endOfLineP :: Atto.Parser Text
endOfLineP =
  (Atto.<?> "end of line") $
    skipHorzSpace
      *> (newline <|> crnl <|> (Atto.endOfInput $> "\n"))
  where
    newline :: Atto.Parser Text
    newline = Atto.char '\n' $> "\n"

    crnl :: Atto.Parser Text
    crnl = do
      _ <- Atto.char '\r'
      _ <- Atto.char '\n'
      pure "\r\n"

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
fenceT :: FenceR -> LTB.Builder
fenceT = cata go
  where
    go :: FenceF LTB.Builder -> LTB.Builder
    go = \case
      BodyF text -> LTB.fromText text
      FenceF Fence {..} ->
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
         in mconcat
              [ replicate fenceIndent (LTB.singleton ' ') & mconcat,
                replicate fenceCount (LTB.singleton char) & mconcat,
                attrs,
                LTB.fromText fenceLineEnd,
                mconcat fenceBody,
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
-- [BodyF "foo", BodyF "bar", FenceF, BodyF "baz"] ->
-- [BodyF "foobar", FenceF, BodyF "baz"]
-- @
--
-- @since 0.5.0.0
concatAdjacentText :: FenceR -> FenceR
concatAdjacentText = cata go
  where
    go :: FenceF FenceR -> FenceR
    go = \case
      b@BodyF {} -> Fix b
      FenceF f@Fence {} -> Fix (FenceF f {fenceBody = body (fenceBody f)})

    -- Process the body of a 'FenceF' value.
    body :: [FenceR] -> [FenceR]
    body =
      foldr accum (Nothing, []) >>> \case
        (Just t, fs) -> Fix (BodyF t) : fs
        (Nothing, fs) -> fs

    -- Accumulate lines of text into the largest group possible.
    accum :: FenceR -> (Maybe Text, [FenceR]) -> (Maybe Text, [FenceR])
    accum (Fix f) (text, fs) = case f of
      BodyF t -> (Just t <> text, fs)
      FenceF {} -> case text of
        Nothing -> (Nothing, Fix f : fs)
        Just t -> (Nothing, Fix f : (Fix $ BodyF t) : fs)
