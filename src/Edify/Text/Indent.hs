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
module Edify.Text.Indent
  ( stripLeadingIndent,
  )
where

import Data.Char (isSpace)
import qualified Data.Text as Text

-- | Remove indentation found in the given text using the amount of
-- leading white-space from the first line as a guide for the rest of
-- the text.
--
-- @since 0.5.0.0
stripLeadingIndent :: Text -> Text
stripLeadingIndent =
  Text.lines
    >>> strip
    >>> Text.unlines
  where
    -- Remove leading indentation from all lines.
    strip :: [Text] -> [Text]
    strip [] = []
    strip lines@(topLine : _) =
      let amount = indentation topLine
       in map (Text.drop amount) lines
    -- Calculate the amount of indentation to remove.
    indentation :: Text -> Int
    indentation = Text.takeWhile isSpace >>> Text.length
