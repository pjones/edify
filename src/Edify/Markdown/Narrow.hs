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
module Edify.Markdown.Narrow
  ( Chunk (..),
    Heading (..),
    toChunks,
    fromChunks,
    narrow,

    -- * Errors
    Error (..),
    renderError,
  )
where

import qualified Data.Attoparsec.Text.Lazy as Atto
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Text.Lazy.Builder as LTB
import Edify.Markdown.Attributes
import Edify.Markdown.Heading
import Edify.Text.JSON
import Edify.Text.Narrow (Token (..))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Util as PP

-- | Errors that may occur.
--
-- @since 0.5.0.0
data Error
  = ParseError String
  | NarrowTokenMissingError Text
  deriving (Generic, Show)

-- | Render an 'Error' for displaying to a user.
--
-- @since 0.5.0.0
renderError :: Error -> PP.Doc ann
renderError = \case
  ParseError s ->
    PP.fillSep
      [ PP.reflow "Markdown parsing error:",
        PP.pretty s
      ]
  NarrowTokenMissingError t ->
    PP.fillSep
      [ PP.reflow "While narrowing markdown,",
        PP.reflow "document doesn't have a heading with the requested ID:",
        PP.dquotes (PP.pretty t)
      ]

-- | A single chunk of Markdown.
--
-- @since 0.5.0.0
data Chunk
  = Chunk Text
  | CHeading Heading
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via GenericJSON Chunk

-- | Parse the given input and produce a 'Chunk' list.
--
-- @since 0.5.0.0
toChunks :: LText -> Either Error [Chunk]
toChunks =
  Atto.parse parser >>> \case
    Atto.Done _ cs ->
      Right cs
    Atto.Fail _ [] msg ->
      Left (ParseError msg)
    Atto.Fail _ ctx msg ->
      (intercalate " > " ctx <> ": " <> msg)
        & ParseError
        & Left
  where
    parser :: Atto.Parser [Chunk]
    parser =
      Atto.many1
        ( Atto.choice
            [ headingP <&> CHeading,
              chunk <&> Chunk
            ]
        )
        <* Atto.endOfInput
    chunk :: Atto.Parser Text
    chunk = do
      ts <- many (Atto.satisfy (Atto.isEndOfLine >>> not))
      nl <- Atto.many1 (Atto.satisfy Atto.isEndOfLine)
      pure (toText ts <> toText nl)

-- | Convert a 'Chunk' list back to 'LText'.
--
-- @since 0.5.0.0
fromChunks :: [Chunk] -> LText
fromChunks = foldMap go >>> LTB.toLazyText
  where
    go :: Chunk -> LTB.Builder
    go = \case
      Chunk t -> LTB.fromText t
      CHeading h -> headingT h

-- | Narrow Markdown so that it only includes the named heading and
-- its children.
--
-- @since 0.5.0.0
narrow :: Token -> LText -> Either Error LText
narrow (Token token) input = do
  cs <- toChunks input
  foldl' go (mempty, Nothing) cs & \case
    ([], _) -> Left (NarrowTokenMissingError token)
    (xs, _) -> Right (fromChunks $ reverse xs)
  where
    -- Narrow a list of chucks via a fold.
    go :: ([Chunk], Maybe Int) -> Chunk -> ([Chunk], Maybe Int)
    go (cs, level) c =
      case c of
        Chunk _ -> (maybe cs (const (c : cs)) level, level)
        CHeading Heading {..} ->
          case level of
            Nothing ->
              if tokenMatches (headingAttrs >>= attrID)
                then (c : cs, Just headingLevel)
                else (cs, Nothing)
            Just n ->
              if headingLevel > n
                then (c : cs, level)
                else (cs, Nothing)

    tokenMatches :: Maybe Name -> Bool
    tokenMatches = \case
      Nothing -> False
      Just hid -> CaseInsensitive.mk token == CaseInsensitive.mk (toText hid)
