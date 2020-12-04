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
-- Pandoc-like attribute blocks.
module Edify.Markdown.Attributes
  ( -- * Attribute Parsing and Generating
    Attributes (..),
    attributesP,
    attributesT,
    attributesShortcutT,

    -- * Attribute Names
    Name,
    toName,
    nameP,
    nameT,

    -- * Attribute Values
    Value,
    toValue,
    valueP,
    valueT,

    -- * CSS Class Names
    CssIdent,
    toCssIdent,
    cssIdentP,
    cssIdentT,
  )
where

import Control.Lens (at, (%~), (?~))
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text.Lazy as Atto
import Data.Char (isAlpha, isAlphaNum, isAscii, isHexDigit, isLetter, isPrint, isSpace)
import Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Read as Text
import qualified Edify.HTML.Entities as HTML
import Edify.JSON
import Text.Printf (printf)

-- | A attribute name according to HTML.
--
-- @since 0.5.0.0
newtype Name = Name
  {getAttrName :: Text}
  deriving stock (Generic)
  deriving
    ( Show,
      ToText,
      Eq,
      ToJSON,
      Aeson.ToJSONKey,
      Hashable
    )
    via Text

instance FromJSON Name where
  parseJSON = Aeson.withText "Attribute name" (toName >>> maybe empty pure)

instance Aeson.FromJSONKey Name where
  fromJSONKey = Aeson.FromJSONKeyValue Aeson.parseJSON

-- | Create an 'Name' value.
--
-- @since 0.5.0.0
toName :: Text -> Maybe Name
toName name = do
  (h, t) <- Text.uncons name
  guard (isAttrNameFirstChar h && Text.all isAttrNameOtherChar t)
  pure (Name name)

-- | An attribute value according to HTML.
--
-- @since 0.5.0.0
newtype Value = Value Text
  deriving stock (Generic)
  deriving
    ( Show,
      Eq,
      ToText,
      IsString,
      Semigroup,
      Monoid,
      ToJSON,
      FromJSON
    )
    via Text

-- | Encode the given 'Text' as an attribute value.
--
-- @since 0.5.0.0
toValue :: Text -> Value
toValue = Value

-- | A CSS identifier.
--
-- @since 0.5.0.0
newtype CssIdent = CssIdent
  {getCssIdent :: Text}
  deriving stock (Generic)
  deriving (Show, Eq, ToText, ToJSON, FromJSON) via Text

-- | Encode the given 'Text' as a CSS class name.
--
-- @since 0.5.0.0
toCssIdent :: Text -> Maybe CssIdent
toCssIdent t
  | Text.null t = Nothing
  | otherwise = Just (CssIdent t)

-- | An attribute block (a la Pandoc).
--
-- @since 0.5.0.0
data Attributes = Attributes
  { -- | Optional ID field (e.g., #this-is-an-id).
    attrID :: Maybe Name,
    -- | Optional (zero or more) class names (e.g., .class-one
    -- .class-two).
    attrClasses :: [CssIdent],
    -- | Key-value pairs (e.g., key=value foo="dark green").
    attrPairs :: HashMap Name Value
  }
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via GenericJSON Attributes

instance Semigroup Attributes where
  (<>) x y =
    Attributes
      { attrID = attrID x <|> attrID y,
        attrClasses = attrClasses x <> attrClasses y,
        attrPairs = attrPairs x <> attrPairs y
      }

instance Monoid Attributes where
  mempty = Attributes Nothing mempty mempty

-- | Internal type for parsing attributes.
--
-- @since 0.5.0.0
data Attr
  = AttrID Name
  | AttrClass CssIdent
  | AttrKeyVal Name Value

-- | Parse Pandoc-style attribute blocks.
--
-- @since 0.5.0.0
attributesP :: Atto.Parser Attributes
attributesP =
  Atto.skipWhile Atto.isHorizontalSpace
    *> ( (Atto.string "{-}" $> Attributes Nothing [CssIdent "unnumbered"] mempty)
           <|> inBracesP
       )
    <* Atto.skipWhile Atto.isHorizontalSpace
  where
    inBracesP :: Atto.Parser Attributes
    inBracesP = do
      Atto.char '{' *> Atto.skipSpace
      attrs <- Atto.sepBy attrP (Atto.many1 Atto.space)
      Atto.skipSpace <* Atto.char '}'
      pure $
        foldr
          ( \case
              AttrID t -> #attrID ?~ t
              AttrClass t -> #attrClasses %~ (t :)
              AttrKeyVal k v -> #attrPairs . at k ?~ v
          )
          (Attributes Nothing mempty mempty)
          attrs
    attrP :: Atto.Parser Attr
    attrP = Atto.choice [idP, classP, kvP]
    idP :: Atto.Parser Attr
    idP =
      Atto.char '#'
        *> nameP
        <&> AttrID
    classP :: Atto.Parser Attr
    classP =
      Atto.char '.'
        *> cssIdentP
        <&> AttrClass
    kvP :: Atto.Parser Attr
    kvP =
      let sep = Atto.skipSpace *> Atto.char '=' <* Atto.skipSpace
       in AttrKeyVal
            <$> (nameP <* sep)
            <*> valueP

-- | Encode an 'Attributes' value as a 'LTB.Builder'.
--
-- @since 0.5.0.0
attributesT :: Attributes -> LTB.Builder
attributesT attrs =
  mconcat
    [ LTB.singleton '{',
      LTB.fromLazyText (toAttrs attrs),
      LTB.singleton '}'
    ]
  where
    toAttrs :: Attributes -> LText
    toAttrs Attributes {..} =
      LText.intercalate
        " "
        ( maybe mempty (idT >>> one) attrID
            <> map classT attrClasses
            <> HashMap.foldrWithKey kvT mempty attrPairs
        )

    idT :: Name -> LText
    idT = nameT >>> ("#" <>)

    classT :: CssIdent -> LText
    classT = cssIdentT >>> ("." <>)

    kvT :: Name -> Value -> [LText] -> [LText]
    kvT k v ts = (toLazy (getAttrName k) <> "=" <> valueT v) : ts

-- | Like 'attributesT' except the caller has more control over how
-- the attribute set will be encoded.
--
-- @since 0.5.0.0
attributesShortcutT ::
  -- | Function called when the attributes only contain a single class
  -- name.  The encoded class name is passed as an argument.
  (LTB.Builder -> LTB.Builder) ->
  -- | Function called when the attributes are completely empty.  The
  -- attribute set is encoded as an empty set of braces and passed as
  -- an argument.
  (LTB.Builder -> LTB.Builder) ->
  -- | Function called on a normal set of attributes.  The encoded
  -- attributes are passed as an argument.
  (LTB.Builder -> LTB.Builder) ->
  -- | The attributes to encode.
  Attributes ->
  -- | The final encoding.
  LTB.Builder
attributesShortcutT onShortcut onEmpty onFull attrs
  | attrs == mempty = onEmpty (attributesT attrs)
  | otherwise = case attrs of
    Attributes Nothing [css] kvs
      | HashMap.null kvs -> onShortcut (LTB.fromLazyText (cssIdentT css))
      | otherwise -> onFull (attributesT attrs)
    _notShortcut -> onFull (attributesT attrs)

-- | Is the given 'Char' a valid first-character for an HTML attribute name?
--
-- @since 0.5.0.0
isAttrNameFirstChar :: Char -> Bool
isAttrNameFirstChar c =
  isLetter c
    || c == '_'
    || c == ':'

-- | Can the given 'Char' appear in an HTML attribute name in a
-- position other than the first character?
--
-- @since 0.5.0.0
isAttrNameOtherChar :: Char -> Bool
isAttrNameOtherChar c =
  isAttrNameFirstChar c
    || isAlphaNum c
    || c == '-'
    || c == '.'

-- | Parse an attribute name according to the HTML rules.
--
-- @since 0.5.0.0
nameP :: Atto.Parser Name
nameP = do
  c <- Atto.satisfy isAttrNameFirstChar
  cs <- many (Atto.satisfy isAttrNameOtherChar)
  pure (Name $ toText (c : cs))

-- | Encode an 'Name' value as 'LText'.
--
-- @since 0.5.0.0
nameT :: Name -> LText
nameT (Name t) = toLazy t

-- | Parse an attribute value according to the HTML rules.
--
-- @since 0.5.0.0
valueP :: Atto.Parser Value
valueP =
  Atto.choice
    [ unquoted,
      quoted '\'',
      quoted '"'
    ]
    <&> Value
  where
    unquoted :: Atto.Parser Text
    unquoted = do
      cs <- Atto.many1 (Atto.satisfy isUnquotedAttributeChar)
      decodeEntities (toText cs)
    quoted :: Char -> Atto.Parser Text
    quoted quote = do
      _ <- Atto.char quote
      cs <- Atto.manyTill Atto.anyChar (Atto.char quote)
      decodeEntities (toText cs)

-- | Encode an 'Value' value as 'LText'.
--
-- @since 0.5.0.0
valueT :: Value -> LText
valueT (Value t)
  | not (Text.null t) && Text.all isUnquotedAttributeChar t =
    toLazy (encodeUnquoted t)
  | otherwise =
    toLazy (quote t)
  where
    quote :: Text -> Text
    quote t = "\"" <> encodeQouted t <> "\""
    encode :: Char -> Text
    encode = \case
      '&' -> "&amp;"
      '"' -> "&quote;"
      c -> "&#x" <> toText (printf "%x" c :: String) <> ";"
    encodeUnquoted :: Text -> Text
    encodeUnquoted =
      let check c =
            if c /= '&' && isUnquotedAttributeChar c
              then one c
              else encode c
       in Text.foldl' (\t c -> t <> check c) mempty
    encodeQouted :: Text -> Text
    encodeQouted =
      let check c =
            case c of
              ' ' -> one c
              '&' -> encode c
              '"' -> encode c
              _
                | isSpace c -> encode c
                | isPrint c -> one c
                | otherwise -> encode c
       in Text.foldl' (\t c -> t <> check c) mempty

-- | Is the given character allowed in an unquoted attribute value?
--
-- @since 0.5.0.0
isUnquotedAttributeChar :: Char -> Bool
isUnquotedAttributeChar c =
  not (isSpace c)
    && c /= '\''
    && c /= '"'
    && c /= '='
    && c /= '<'
    && c /= '>'
    && c /= '`'
    && c /= '}'

-- | Is the given character allowed as the first character in a CSS class?
--
-- @since 0.5.0.0
isCssIdentFirstChar :: Char -> Bool
isCssIdentFirstChar c =
  c == '\\'
    || c == '-'
    || c == '_'
    || (isAscii c && isAlpha c)

-- | Is the given character allowed to be in a CSS class name other
-- than the first character?
--
-- @since 0.5.0.0
isCssIdentOtherChar :: Char -> Bool
isCssIdentOtherChar c =
  isCssIdentFirstChar c
    || (isAscii c && isAlphaNum c)
    || ord c >= 0x00a0

-- | Parse a CSS identifier.
--
-- <https://www.w3.org/TR/CSS21/syndata.html#characters>
--
-- @since 0.5.0.0
cssIdentP :: Atto.Parser CssIdent
cssIdentP = do
  c <- Atto.satisfy isCssIdentFirstChar
  cs <- if c == '\\' then escapedP else pure (one c)
  rest <-
    many
      ( Atto.choice
          [ normalP,
            Atto.char '\\' *> escapedP
          ]
      )
  pure (CssIdent (cs <> mconcat rest))
  where
    normalP :: Atto.Parser Text
    normalP =
      Atto.many1 (Atto.satisfy (\c -> c /= '\\' && isCssIdentOtherChar c))
        <&> toText
    escapedP :: Atto.Parser Text
    escapedP = escapedSingleNonHexP <|> escapedHexP
    escapedSingleNonHexP :: Atto.Parser Text
    escapedSingleNonHexP = Atto.satisfy (isHexDigit >>> not) <&> one
    escapedHexP :: Atto.Parser Text
    escapedHexP = do
      hexcodeP <&> Text.hexadecimal >>= \case
        Left _ -> mempty
        Right (c, t)
          | Text.null t -> pure (if c == 0 then mempty else one (chr c))
          | otherwise -> pure mempty
    -- Hex-encoded character that will be terminated by a space, a
    -- non-hexadecimal character, or after 6 hex digits.  Fails if
    -- there isn't at least one character in the returned string.
    hexcodeP :: Atto.Parser Text
    hexcodeP = do
      t <- go 6 <&> toText
      guard (not $ Text.null t)
      pure t
      where
        go :: Int -> Atto.Parser String
        go 0 = pure []
        go n =
          optional (Atto.satisfy (\c -> isHexDigit c || isSpace c)) >>= \case
            Nothing -> pure []
            Just c
              | isSpace c -> do
                when (c == '\r') (void $ optional $ Atto.char '\n')
                pure []
              | otherwise ->
                (c :) <$> go (pred n)

-- | Encode a CSS identifier as 'LText'.
--
-- @since 0.5.0.0
cssIdentT :: CssIdent -> LText
cssIdentT (CssIdent name) =
  let (h, t) = fromMaybe (' ', mempty) $ Text.uncons name
   in LText.foldl'
        (\t c -> t <> check isCssIdentOtherChar c)
        (check isCssIdentFirstChar h)
        (toLazy t)
  where
    check :: (Char -> Bool) -> Char -> LText
    check f c
      | c /= '\\' && f c = one c
      | otherwise = encode c
    encode :: Char -> LText
    encode c = "\\" <> toLText (printf "%06x" c :: String)

-- | Decode HTML entities in a string.
--
-- @since 0.5.0.0
decodeEntities :: Alternative f => Text -> f Text
decodeEntities t
  | Text.null t = pure t
  | otherwise =
    case Atto.parse (parser <* Atto.endOfInput) (toLazy t) of
      Atto.Fail {} -> empty
      Atto.Done _ dt -> pure dt
  where
    parser :: Atto.Parser Text
    parser =
      Atto.many1
        ( Atto.choice
            [ normalP,
              entityP
            ]
        )
        <&> mconcat

    normalP :: Atto.Parser Text
    normalP = Atto.many1 (Atto.satisfy (/= '&')) <&> toText

    entityP :: Atto.Parser Text
    entityP = do
      _ <- Atto.char '&'
      cs <- Atto.many1 (Atto.satisfy (/= ';')) <&> toText
      _ <- Atto.char ';'

      pure $
        fromMaybe mempty $
          case Text.uncons cs of
            Just ('#', code) -> decodeNum code
            _named -> HashMap.lookup cs HTML.entities

    decodeNum :: Text -> Maybe Text
    decodeNum input =
      let decode = \case
            Left _invalid -> Nothing
            Right (n :: Integer, t) -> do
              guard (Text.null t)
              guard (n >= 1 && n <= 0x10FFFF)
              pure (Text.singleton (chr $ fromInteger n))
       in case Text.uncons input of
            Just ('x', num) -> decode (Text.hexadecimal num)
            Just ('X', num) -> decode (Text.hexadecimal num)
            Just _decimal -> decode (Text.decimal input)
            Nothing -> Nothing
