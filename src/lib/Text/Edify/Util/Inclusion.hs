{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
module Text.Edify.Util.Inclusion
  ( FileRef(..)
  , inclusionMarker
  ) where

--------------------------------------------------------------------------------
-- Library imports.
import qualified Data.Attoparsec.Text as Atto
import Data.Attoparsec.Text hiding (parse)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
-- | A path to a file, and optional header ID.
data FileRef = FileRef Text (Maybe Text)
               deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | Try to extract a file name from a string if it contains and
-- inclusion marker.
inclusionMarker :: Text -> Maybe FileRef
inclusionMarker s =
  case Atto.parseOnly inclusionMarkerParser s of
    Left _  -> Nothing
    Right f -> Just (ref $ toText f)

  where
    ref :: Text -> FileRef
    ref path =
      let (name, hid) = Text.span (/= '#') path
      in FileRef name (Text.stripPrefix "#" hid)

--------------------------------------------------------------------------------
-- | Parsec parser for inclusion markers.
inclusionMarkerParser :: Parser FilePath
inclusionMarkerParser = do
  skipSpace
  void (string "<<(")
  many1 (notChar ')') <* char ')'
