{-# LANGUAGE OverloadedStrings #-}

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
import Data.List (span)
import Text.Parsec
import Text.Parsec.String

--------------------------------------------------------------------------------
-- | A path to a file, and optional header ID.
data FileRef = FileRef FilePath (Maybe String)
               deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | Try to extract a file name from a string if it contains and
-- inclusion marker.
inclusionMarker :: String -> Maybe FileRef
inclusionMarker s =
  case runParser inclusionMarkerParser () "" s of
    Left _  -> Nothing
    Right f -> Just (ref f)

  where
    ref :: String -> FileRef
    ref path = case span (/= '#') path of
                 (name, [])  -> FileRef name Nothing
                 (name, hid) -> FileRef name (cleanID hid)
  
    cleanID :: String -> Maybe String
    cleanID [_]      = Nothing
    cleanID ('#':xs) = Just xs
    cleanID _        = Nothing
    
--------------------------------------------------------------------------------
-- | Parsec parser for inclusion markers.
inclusionMarkerParser :: Parser FilePath
inclusionMarkerParser =
  spaces             *>
  string "<<("       *>
  many1 (noneOf ")") <*
  char ')'
