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
-- Simple string interpolation a la format strings.
module Edify.Text.Placeholders
  ( AST,
    parse,
    vars,
    substitute,
  )
where

import qualified Data.Attoparsec.Text as Atto
import Data.Fix (Fix)
import Data.Functor.Foldable (cata, embed)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Lazy.Builder as Builder

-- | Non-recursive AST for format strings.
--
-- @since 0.5.0.0
data AstF r
  = -- | Base case. Marks the end of the template.
    Terminal
  | -- | A block of text.
    PlainText Text r
  | -- | Variable substitution.
    Variable Char r
  deriving stock (Generic, Functor)

-- | Fully recursive AST.
--
-- @since 0.5.0.0
type AST = Fix AstF

-- | Parse a format string into an AST.
--
-- @since 0.5.0.0
astP :: Atto.Parser AST
astP =
  Atto.choice
    [ varP,
      plainP,
      pure (embed Terminal)
    ]
  where
    varP :: Atto.Parser AST
    varP = do
      _ <- Atto.char '%'
      c <- Atto.anyChar <|> pure '%'

      if c == '%'
        then embed . PlainText (one c) <$> astP
        else embed . Variable c <$> astP

    plainP :: Atto.Parser AST
    plainP = do
      text <- Atto.many1 (Atto.satisfy (/= '%')) <&> toText
      embed . PlainText text <$> astP

-- | Perform variable substitution and render the resulting text.
--
-- @since 0.5.0.0
substitute :: HashMap Char Text -> AST -> Text
substitute table = cata go >>> Builder.toLazyText >>> toStrict
  where
    go = \case
      Terminal -> mempty
      PlainText t r -> Builder.fromText t <> r
      Variable c r ->
        case HashMap.lookup c table of
          Nothing ->
            Builder.singleton '%'
              <> Builder.singleton c
              <> r
          Just replacement ->
            Builder.fromText replacement
              <> r

-- | Return a list of variables used in the given 'AST'.
vars :: AST -> [Char]
vars = cata go >>> sortNub
  where
    go = \case
      Terminal -> mempty
      PlainText _text r -> r
      Variable c r -> c : r

-- | Parse a format string.
--
-- @since 0.5.0.0
parse :: Text -> Either String AST
parse = Atto.parseOnly astP
