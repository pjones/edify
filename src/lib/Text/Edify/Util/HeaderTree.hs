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
       , HeaderTree (..)
       , headerTree
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Markdown

--------------------------------------------------------------------------------
-- | Information about a single header.
data HeaderInfo = HeaderInfo
  { level :: Int
  , attrs :: Attr
  , title :: String
  } deriving Show

--------------------------------------------------------------------------------
-- | A recursive, tree-like structure of headers.
data HeaderTree = HeaderTree HeaderInfo [HeaderTree] deriving Show

--------------------------------------------------------------------------------
-- | Turn a @Pandoc@ into a list of @HeaderTree@ values.  The list
-- forms a hierarchy were the elements roots of the tree.
headerTree :: Pandoc -> [HeaderTree]
headerTree = go . map (\h -> HeaderTree h []) . onlyHeaders
  where
    go :: [HeaderTree] -> [HeaderTree]
    go [] = []
    go (x:xs) = let (y,ys) = place x xs in y:go ys

    place :: HeaderTree -> [HeaderTree] -> (HeaderTree, [HeaderTree])
    place h [] = (h, [])
    place (HeaderTree h hs) (x@(HeaderTree h' _):xs)
      | level h' > level h = let (y,ys) = place x xs
                             in place (HeaderTree h (hs ++ [y])) ys
      | otherwise          = (HeaderTree h hs, x:xs)

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
