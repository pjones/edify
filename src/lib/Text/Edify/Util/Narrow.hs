{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
module Text.Edify.Util.Narrow
       ( narrow
       , narrowToToken
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import Control.Monad
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- | Narrow the given text using the default token delimiters.
narrow :: Text -> Text -> Maybe Text
narrow token = narrowToToken ("<<: " <> token <> "\n", ":>>\n")

--------------------------------------------------------------------------------
-- | Narrow the given text to a beginning and ending delimiter.
narrowToToken :: (Text, Text) -> Text -> Maybe Text
narrowToToken (start, end) = matchEnd <=< matchStart
  where
    matchStart :: Text -> Maybe Text
    matchStart txt = case T.breakOn start txt of
      (_, match) | T.null match -> Nothing
                 | otherwise    -> Just $! stripStart match

    matchEnd :: Text -> Maybe Text
    matchEnd txt = case T.breakOn end txt of
      (prefix, match) | T.null match -> Nothing
                      | otherwise    -> Just $! stripEnd prefix

    stripStart :: Text -> Text
    stripStart = T.drop (T.length start)

    stripEnd :: Text -> Text
    stripEnd = T.dropWhileEnd (/= '\n')
