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
    RewriteError (..),
    Rewritten,
    rewrite,
    discardMatchingDivs,
  )
where

import Control.Lens ((.~), (^.), _2)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Char as Char
import Data.Functor.Foldable (Fix (..), cata, embed, project)
import Data.Generics.Labels ()
import qualified Data.HashSet as HashSet
import Data.Semigroup (Max (..))
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as LTB
import Edify.JSON
import Edify.Markdown.Attributes (Attributes)
import qualified Edify.Markdown.Attributes as Attrs
import Edify.Markdown.Common (endOfLineP, skipHorzSpace, wholelineP)
import qualified Edify.Text.Indent as Indent

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

-- | Track what type of DIV was parsed.
--
-- @since 0.5.0.0
data DivStyle
  = -- | Newer fenced DIVs.
    FenceStyleDiv
  | -- | Traditional HTML DIVs.
    ElementStyleDiv
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via GenericJSON DivStyle

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
    DivFence DivStyle Props [r]
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
fenceP p = (divFenceP p <|> divElementP p <|> codeP) <&> concatAdjacentText

-- | Div blocks using the fence style.
divFenceP :: Atto.Parser t -> Atto.Parser (Fence t)
divFenceP parser = (Atto.<?> "fenced div block") $ do
  (indent, colons, char) <- fenceCharsP (== ':')
  attrs <- barewordP <|> Attrs.attributesP
  lineEnd <-
    skipHorzSpace
      *> Atto.skipWhile (== char)
      *> endOfLineP
  body <-
    Atto.manyTill
      (divBodyP parser)
      (closingP char indent 3)
  let props =
        Props
          { fenceIndent = indent,
            fenceCount = colons,
            fenceLineEnd = lineEnd,
            fenceAttrs = attrs
          }
  pure $ embed (DivFence FenceStyleDiv props body)

-- | Div blocks using the HTML element style.
divElementP :: Atto.Parser t -> Atto.Parser (Fence t)
divElementP parser = (Atto.<?> "HTML div block") $ do
  indent <- many (Atto.satisfy Atto.isHorizontalSpace) <&> length
  _ <- Atto.string "<div"
  attrs <-
    Atto.choice
      [ Atto.satisfy Char.isSpace *> (many Attrs.keyValueP <&> Attrs.kvToAttrs),
        pure mempty
      ]
  lineEnd <- Atto.char '>' *> (endOfLineP <|> pure "\n")
  body <- Atto.manyTill (divBodyP parser) (skipHorzSpace *> Atto.string "</div>")
  void endOfLineP <|> pass
  let props =
        Props
          { fenceIndent = indent,
            fenceCount = 0,
            fenceLineEnd = lineEnd,
            fenceAttrs = attrs
          }
  pure $ embed (DivFence ElementStyleDiv props body)

-- | Parse the body of a DIV.
divBodyP :: Atto.Parser t -> Atto.Parser (Fence t)
divBodyP parser =
  Atto.choice
    [ divFenceP parser,
      divElementP parser,
      codeP,
      embed . FenceBody <$> parser
    ]

-- | Fenced code blocks.
codeP :: Atto.Parser (Fence t)
codeP = (Atto.<?> "fenced code block") $ do
  (indent, chars, char) <- fenceCharsP (\c -> c == '`' || c == '~')
  attrs <- barewordP <|> Attrs.attributesP <|> pure mempty
  lineEnd <- endOfLineP
  body <- Atto.manyTill wholelineP (closingP char indent chars)
  let props =
        Props
          { fenceIndent = indent,
            fenceCount = chars,
            fenceLineEnd = lineEnd,
            fenceAttrs = attrs
          }
  pure $
    embed $
      CodeFence char props (mconcat body)

-- | The closing of a fence block.
closingP ::
  -- | The character used to mark the fence.
  Char ->
  -- | The indentation level.
  Int ->
  -- | Minimum number of times the fence character need to be present.
  Int ->
  -- | The parser that returns unit.
  Atto.Parser ()
closingP char indent count =
  Atto.count indent (Atto.satisfy Atto.isHorizontalSpace)
    *> atLeastP count (== char)
    *> (endOfLineP $> ())

-- | Parse the start of a fence with the given fence character.
--
-- Returns:
--
--  * @_1@: Indentation level
--  * @_2@: Number of fence characters
--  * @_3@: The fence character used.
fenceCharsP :: (Char -> Bool) -> Atto.Parser (Int, Int, Char)
fenceCharsP pred = do
  indent <- many (Atto.satisfy Atto.isHorizontalSpace) <&> length
  (chars, char) <- atLeastP 3 pred <* skipHorzSpace
  pure (indent, chars, char)

-- | Read /N/ copies of a 'Char', then any remaining instances.
atLeastP :: Int -> (Char -> Bool) -> Atto.Parser (Int, Char)
atLeastP n pred = (Atto.<?> ("at least " <> show n <> " fence characters")) $ do
  c <- Atto.satisfy pred
  _ <- Atto.count (n - 1) (Atto.char c)
  cs <- many (Atto.char c)
  pure (length cs + n, c)

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
      DivFence FenceStyleDiv props@Props {fenceAttrs} body ->
        let attrs = Attrs.attributesShortcutT id id id fenceAttrs
         in format ':' props (LTB.singleton ' ' <> attrs) (mconcat body)
      DivFence ElementStyleDiv Props {..} body ->
        mconcat
          [ replicate fenceIndent (LTB.singleton ' ') & mconcat,
            "<div",
            if fenceAttrs == mempty
              then mempty
              else
                LTB.singleton ' '
                  <> Attrs.keyValueT (Attrs.kvFromAttrs fenceAttrs),
            LTB.singleton '>',
            LTB.fromText fenceLineEnd,
            mconcat body,
            replicate fenceIndent (LTB.singleton ' ') & mconcat,
            "</div>",
            LTB.fromText fenceLineEnd
          ]

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
        DivFence style props body ->
          DivFence style props $
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
        DivFence style p b -> DivFence style p <$> sequenceA b

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
        DivFence style props body ->
          DivFence style
            <$> #fenceAttrs f props
            <*> sequenceA body

-- | Instructs the 'rewrite' function how to rewrite portions of a
-- recursive fence structure.
--
-- @since 0.5.0.0
data Rewrite
  = -- | Rewrite parts of a fence.
    Rewrite
      { -- | When 'Just', update fence attributes.
        rewriteAttrs :: Maybe Attrs.Attributes,
        -- | When 'Just', replace the fence's body.  If there are fences
        -- nested under this fence then they will *not* be preserved.
        rewriteBody :: Maybe Text
      }
  | -- | Completely remove this fence block.
    Discard
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via GenericJSON Rewrite

instance Semigroup Rewrite where
  (<>) (Rewrite x1 y1) (Rewrite x2 y2) =
    Rewrite (x1 <|> x2) (y1 <|> y2)
  (<>) Discard _other = Discard
  (<>) _other Discard = Discard

instance Monoid Rewrite where
  mempty = Rewrite Nothing Nothing

-- | Return a rewrite request that will discard a fence if it contains
-- one of the given CSS classes.
--
-- @since 0.5.0.0
discardMatchingDivs ::
  HashSet (CaseInsensitive.CI Attrs.CssIdent) ->
  Attrs.Attributes ->
  Rewrite
discardMatchingDivs toRemove attrs =
  let classes =
        HashSet.fromList $
          map CaseInsensitive.mk (Attrs.attrClasses attrs)
   in if any (`HashSet.member` classes) toRemove
        then Discard
        else mempty

-- | Error information when a rewrite fails.
--
-- @since 0.5.0.0
data RewriteError = RewriteError
  { -- | The original fence rendered as text.
    errorRenderedFence :: !Text,
    -- | The failed rewrite request.
    errorRewriteRequest :: !Rewrite,
    -- | The associated error message.
    errorMessage :: !String
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via GenericJSON RewriteError

-- | Helper type mostly to reduce keyboard typing.
--
-- The result of performing a rewrite is either:
--
--   * @Left@: an error occurred
--   * @Right Nothing@: Successful rewrite, but fence was discarded.
--   * @Right Just@: Successful rewrite with new fence.
--
-- @since 0.5.0.0
type Rewritten t = Either RewriteError (Maybe (Fence t))

-- | A traversal that can rewrite portions of the recursive fence
-- structure.
--
-- @since 0.5.0.0
rewrite ::
  forall t f.
  Semigroup t =>
  Monad f =>
  -- | The width of a tab character.  Used to manipulate body
  -- indentation.
  Indent.Tabstop ->
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
rewrite tabstop encode parser f = cata go
  where
    go :: FenceF t (f (Rewritten t)) -> f (Rewritten t)
    go = \case
      FenceBody b -> pure (Right $ Just $ embed (FenceBody b))
      CodeFence char props body -> do
        f (fenceAttrs props, Indent.unindent tabstop body) >>= \case
          Rewrite {..} ->
            case rewriteBody of
              Nothing ->
                CodeFence char (attrs rewriteAttrs props) body
                  & embed
                  & Just
                  & Right
                  & pure
              Just b ->
                fixupCodeBody tabstop char (attrs rewriteAttrs props) b
                  & Just
                  & Right
                  & pure
          Discard ->
            pure (Right Nothing)
      DivFence style props body ->
        sequenceA body <&> sequenceA
          >>= either
            (pure . Left)
            ( catMaybes >>> \bodies -> do
                let text =
                      foldMap (fenceT encode) bodies
                        & LTB.toLazyText
                        & toStrict
                    rendered =
                      DivFence style props [embed $ FenceBody text]
                        & embed
                        & fenceT LTB.fromText
                        & LTB.toLazyText
                        & toStrict
                f (fenceAttrs props, Indent.unindent tabstop text) >>= \case
                  request@Rewrite {..} ->
                    case rewriteBody of
                      Nothing ->
                        DivFence style (attrs rewriteAttrs props) bodies
                          & embed
                          & Just
                          & Right
                          & pure
                      Just b ->
                        Indent.indent (fenceIndent props) tabstop b
                          & parse style (attrs rewriteAttrs props)
                          & bimap (RewriteError rendered request) Just
                          & pure
                  Discard ->
                    pure (Right Nothing)
            )

    -- Optionally update the attributes inside the given 'Props'.
    attrs :: Maybe Attributes -> Props -> Props
    attrs = \case
      Nothing -> id
      Just a -> #fenceAttrs .~ a

    -- Parse the body of a div fence and reconstruct it.
    parse :: DivStyle -> Props -> Text -> Either String (Fence t)
    parse style props body =
      let p = Atto.many1 (divBodyP parser)
       in Atto.parseOnly (p <* Atto.endOfInput) body
            <&> (DivFence style props >>> embed >>> concatAdjacentText)

-- | Ensure the body of a fenced code block is well formed.
-- Specifically, if the body contains fence characters, force the fence
-- to have more than the body.  Also indents the body correctly.
--
-- @since 0.5.0.0
fixupCodeBody :: Indent.Tabstop -> Char -> Props -> Text -> Fence t
fixupCodeBody tabstop char props text =
  let body =
        Indent.indent (fenceIndent props) tabstop text
          & ensureFinalNewline
      count = maxFalseFenceLength body
      props' =
        if count >= fenceCount props
          then props {fenceCount = count + 1}
          else props
   in embed $ CodeFence char props' body
  where
    -- Count the number of fence characters per line in the body.
    maxFalseFenceLength :: Text -> Int
    maxFalseFenceLength text =
      fromRight 0 $
        (`Atto.parseOnly` text) $
          many
            ( Atto.choice
                [ (fenceCharsP (== char) <&> (^. _2)) <* wholelineP,
                  wholelineP $> 0
                ]
            )
            >>= \case
              [] -> pure 0
              ns -> foldMap Max ns & getMax & pure
    ensureFinalNewline :: Text -> Text
    ensureFinalNewline text =
      Text.unsnoc text & \case
        Just (t, c)
          | c == '\n' -> text
          | otherwise -> t <> one c <> fenceLineEnd props
        Nothing -> text
