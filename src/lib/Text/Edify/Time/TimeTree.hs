{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Summarizes time codes attached to Markdown headers.
module Text.Edify.Time.TimeTree
       ( TimeTree (..)
       , timeTree
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import qualified Data.Text as T
import           Text.Pandoc.Definition

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Time.TimeCode
import Text.Edify.Util.HeaderTree

--------------------------------------------------------------------------------
-- | Represent a tree of headers with time code information.  Only
-- leaf headers are allowed to contain time code data and header notes
-- will be assigned the total time taken by their children.
data TimeTree = TimeTree HeaderInfo TimeCode [TimeTree]

--------------------------------------------------------------------------------
-- | Convert a list of @HeaderTree@ values to a list of @TimeTree@
-- values, with the possibility of failure if time codes can't be
-- parsed or are missing.  Time codes are taken from the header
-- attribute given in the first argument.
timeTree :: String -> [HeaderTree] -> Either String [TimeTree]
timeTree _ [] = Right []
timeTree key ((HeaderTree hi hs):xs) = do
    others <- timeTree key xs

    case (hs, getTimeCode (attrs hi)) of
      ([], Nothing) -> Left ("header missing time code: " ++ title hi)
      ([], Just tc) -> do timeCode <- parse (T.pack tc)
                          return ((TimeTree hi timeCode []):others)

      (_,   Just _)  -> Left ("non-leaf header with time code: " ++ title hi)
      (hs', Nothing) -> do below <- timeTree key hs'
                           let total = sum $ map (\(TimeTree _ tc _) -> tc) below
                           return ((TimeTree hi total below):others)

  where
    getTimeCode :: Attr -> Maybe String
    getTimeCode (_, _, alist)= lookup key alist
