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
data TimeTree a = TimeTree (HeaderInfo a) TimeCode [TimeTree a]

--------------------------------------------------------------------------------
-- | TimeCode lookup function.
type Lookup a = (HeaderInfo a -> Either String TimeCode)

--------------------------------------------------------------------------------
-- | Convert a list of @HeaderTree@ values to a list of @TimeTree@
-- values, with the possibility of failure if time codes can't be
-- parsed or are missing.  Time codes should be found in the heading's
-- attributes under the given key.
timeTreeFromAttr :: (Show a)
                 => String
                 -> [HeaderTree a]
                 -> Either String [TimeTree a]
timeTreeFromAttr key = timeTree (timeCodeFromAttr key)

--------------------------------------------------------------------------------
-- | Convert a list of @HeaderTree@ values to a list of @TimeTree@
-- values, with the possibility of failure if time codes can't be
-- parsed or are missing.  Time codes should be found in the given map
-- under heading IDs.
timeTreeFromMap :: (Show a)
                => Map String TimeCode
                -> [HeaderTree a]
                -> Either String [TimeTree a]
timeTreeFromMap m = timeTree (timeCodeFromMap m)

--------------------------------------------------------------------------------
-- | Convert a list of @HeaderTree@ values to a list of @TimeTree@
-- values, with the possibility of failure if time codes can't be
-- parsed or are missing.  Time codes should be found in the given map
-- or in the heading's attributes under the given key.
timeTreeFromMapOrAttr :: (Show a)
                      => Map String TimeCode
                      -> String
                      -> [HeaderTree a]
                      -> Either String [TimeTree a]
timeTreeFromMapOrAttr m key = timeTree (timeCodeFromMapOrAttr m key)

--------------------------------------------------------------------------------
-- | Convert a list of @HeaderTree@ values to a list of @TimeTree@
-- values, with the possibility of failure if time codes can't be
-- parsed or are missing.  Time codes are taken from the given look-up
-- function.
timeTree :: (Show a) => Lookup a -> [HeaderTree a] -> Either String [TimeTree a]
timeTree _ []                      = Right []
timeTree f (HeaderTree hi hs:xs) = do
  others <- timeTree f xs

  case (hs, f hi) of
    -- Leafs.  Must have time codes.
    ([], Left e)   -> Left e
    ([], Right tc) -> return (TimeTree hi tc []:others)

    -- Nodes.  Must not have time codes.
    (_,  Right _)  -> Left ("non-leaf header with time code: " ++ show (title hi))
    (hs', Left _)  -> do
      below <- timeTree f hs'
      let total = sum $ map (\(TimeTree _ tc _) -> tc) below
      return (TimeTree hi total below:others)

--------------------------------------------------------------------------------
timeCodeFromMap :: Map String TimeCode -> HeaderInfo a -> Either String TimeCode
timeCodeFromMap m (HeaderInfo _ (hid, _, _) _) =
  maybeToEither ("no time code for " ++ hid) (M.lookup hid m)

--------------------------------------------------------------------------------
timeCodeFromAttr :: (Show a) => String -> HeaderInfo a -> Either String TimeCode
timeCodeFromAttr key (HeaderInfo _ (_, _, alist) title') = do
  str <- maybeToEither ("no time code for: " ++ show title') (lookup key alist)
  parse (T.pack str)

--------------------------------------------------------------------------------
timeCodeFromMapOrAttr :: (Show a)
                      => Map String TimeCode
                      -> String
                      -> HeaderInfo a
                      -> Either String TimeCode
timeCodeFromMapOrAttr m key header =
  timeCodeFromMap m header `eitherAlt` timeCodeFromAttr key header
