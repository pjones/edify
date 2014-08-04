{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Functions for changing the indentation level of text.
module Text.Edify.Util.Indent
       ( removeIndent
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- | Remove indentation found in the code snippet based on the first line.
removeIndent :: Text -> Text
removeIndent txt = T.unlines . strip (indent txt) . T.lines $ txt
  where
    indent :: Text -> Int
    indent = T.length . T.takeWhile isSpace

    strip :: Int -> [Text] -> [Text]
    strip n = map (T.drop n)
