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
       , timeTreeFromAttr
       , timeTreeFromMap
       , timeTreeFromMapOrAttr
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Time.TimeCode
import Text.Edify.Util.Error (maybeToEither, eitherAlt)
import Text.Edify.Util.HeaderTree

--------------------------------------------------------------------------------
-- | Represent a tree of headers with time code information.  Only
-- leaf headers are allowed to contain time code data and header notes
-- will be assigned the total time taken by their children.
data TimeTree = TimeTree HeaderInfo TimeCode [TimeTree]

--------------------------------------------------------------------------------
-- | TimeCode lookup function.
type Lookup = (HeaderInfo -> Either String TimeCode)

--------------------------------------------------------------------------------
-- | Convert a list of @HeaderTree@ values to a list of @TimeTree@
-- values, with the possibility of failure if time codes can't be
-- parsed or are missing.  Time codes should be found in the heading's
-- attributes under the given key.
timeTreeFromAttr :: String
                 -> [HeaderTree]
                 -> Either String [TimeTree]
timeTreeFromAttr key = timeTree (timeCodeFromAttr key)

--------------------------------------------------------------------------------
-- | Convert a list of @HeaderTree@ values to a list of @TimeTree@
-- values, with the possibility of failure if time codes can't be
-- parsed or are missing.  Time codes should be found in the given map
-- under heading IDs.
timeTreeFromMap :: Map String TimeCode
                -> [HeaderTree]
                -> Either String [TimeTree]
timeTreeFromMap m = timeTree (timeCodeFromMap m)

--------------------------------------------------------------------------------
-- | Convert a list of @HeaderTree@ values to a list of @TimeTree@
-- values, with the possibility of failure if time codes can't be
-- parsed or are missing.  Time codes should be found in the given map
-- or in the heading's attributes under the given key.
timeTreeFromMapOrAttr :: Map String TimeCode
                      -> String
                      -> [HeaderTree]
                      -> Either String [TimeTree]
timeTreeFromMapOrAttr m key = timeTree (timeCodeFromMapOrAttr m key)

--------------------------------------------------------------------------------
-- | Convert a list of @HeaderTree@ values to a list of @TimeTree@
-- values, with the possibility of failure if time codes can't be
-- parsed or are missing.  Time codes are taken from the given look-up
-- function.
timeTree :: Lookup -> [HeaderTree] -> Either String [TimeTree]
timeTree _ []                      = Right []
timeTree f ((HeaderTree hi hs):xs) = do
  others <- timeTree f xs

  case (hs, f hi) of
    -- Leafs.  Must have time codes.
    ([], Left e)   -> Left e
    ([], Right tc) -> return ((TimeTree hi tc []):others)

    -- Nodes.  Must not have time codes.
    (_,  Right _)  -> Left ("non-leaf header with time code: " ++ title hi)
    (hs', Left _)  -> do
      below <- timeTree f hs'
      let total = sum $ map (\(TimeTree _ tc _) -> tc) below
      return ((TimeTree hi total below):others)

--------------------------------------------------------------------------------
timeCodeFromMap :: Map String TimeCode -> HeaderInfo -> Either String TimeCode
timeCodeFromMap m (HeaderInfo _ (hid, _, _) _) =
  maybeToEither ("no time code for " ++ hid) (M.lookup hid m)

--------------------------------------------------------------------------------
timeCodeFromAttr :: String -> HeaderInfo -> Either String TimeCode
timeCodeFromAttr key (HeaderInfo _ (_, _, alist) title') = do
  str <- maybeToEither ("no time code for: " ++ title') (lookup key alist)
  parse (T.pack str)

--------------------------------------------------------------------------------
timeCodeFromMapOrAttr :: Map String TimeCode
                      -> String
                      -> HeaderInfo
                      -> Either String TimeCode
timeCodeFromMapOrAttr m key header =
  timeCodeFromMap m header `eitherAlt` timeCodeFromAttr key header
