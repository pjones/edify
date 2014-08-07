{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Collect Pandoc headers into a tree-like structure.
module Text.Edify.Util.HeaderTree
       ( HeaderInfo (..)
       , onlyHeaders
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Markdown

--------------------------------------------------------------------------------
data HeaderInfo = HeaderInfo Int Attr String deriving Show

--------------------------------------------------------------------------------
--data HeaderTree = Roots [HeaderTree] | Node

--------------------------------------------------------------------------------
onlyHeaders :: Pandoc -> [HeaderInfo]
onlyHeaders = query go
  where
    go :: Block -> [HeaderInfo]
    go (Header n a content) = [HeaderInfo n a (rendered content)]
    go _                    = []

    rendered :: [Inline] -> String
    rendered content = writeMarkdown options (Pandoc nullMeta [Plain content])

    options ::  WriterOptions
    options = def
