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
-- Include one Markdown file in another.
module Edify.Markdown.Include
  ( Include (..),
    includeP,
    includeT,
  )
where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as Text
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Edify.JSON
import Edify.Markdown.Common (endOfLineP, nonindentSpaces)
import Edify.Text.Narrow (Token (..))

-- | Details about an inclusion marker.
--
-- @since 0.5.0.0
data Include = Include
  { includeFile :: FilePath,
    includeToken :: Maybe Token,
    includeEndOfLine :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via GenericJSON Include

-- | Parse an inclusion marker and separate it into a file path and
-- token.
--
-- @since 0.5.0.0
includeP :: Atto.Parser Include
includeP = do
  _ <- nonindentSpaces

  content <-
    Atto.string "<<("
      *> Atto.many1 (Atto.satisfy (/= ')'))
      <* Atto.char ')'

  eol <- endOfLineP

  let (file, token) = tokenFromFile (toText content)

  pure
    Include
      { includeFile = file,
        includeToken = token,
        includeEndOfLine = eol
      }
  where
    -- Extract a token from a file name by looking for the *last*
    -- occurrence of a @#@ character and using the non-blank text
    -- found after it.
    tokenFromFile :: Text -> (FilePath, Maybe Token)
    tokenFromFile text =
      let (left, right) =
            Text.reverse text
              & Text.span (/= '#')
          toPath text =
            Text.strip text
              & Text.reverse
              & toString
          toToken text =
            if Text.null text
              then Nothing
              else
                Text.strip text
                  & Text.reverse
                  & Token
                  & Just
       in case Text.stripPrefix "#" right of
            Nothing -> (toPath left, Nothing)
            Just path -> (toPath path, toToken left)

-- | Render an 'Include' value.
--
-- @since 0.5.0.0
includeT :: Include -> Builder
includeT Include {..} =
  mconcat
    [ "<<(",
      Builder.fromString includeFile,
      maybe mempty (unToken >>> ("#" <>) >>> Builder.fromText) includeToken,
      Builder.singleton ')',
      Builder.fromText includeEndOfLine
    ]
