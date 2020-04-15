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
       , headerTree'
       , listHeaders
       , onlyHeaders
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import qualified Text.Pandoc.Class as Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Markdown

--------------------------------------------------------------------------------
-- | Information about a single header.
data HeaderInfo a = HeaderInfo
  { level :: Int
  , attrs :: Attr
  , title :: a
  } deriving Show

--------------------------------------------------------------------------------
-- | A recursive, tree-like structure of headers.
data HeaderTree a = HeaderTree (HeaderInfo a) [HeaderTree a] deriving Show

--------------------------------------------------------------------------------
-- | Turn a @Pandoc@ body into a list of @HeaderTree@ values.  The
-- list forms a hierarchy were the elements are roots of the tree.
renderHeaders :: HeaderTree [Inline] -> HeaderTree Text
renderHeaders (HeaderTree info children) =
    HeaderTree (update info) (map renderHeaders children)
  where
    update :: HeaderInfo [Inline] -> HeaderInfo Text
    update hi = hi { title = render (title hi) }

    render :: [Inline] -> Text
    render content =
      case Pandoc.runPure $ writeMarkdown def (Pandoc nullMeta [Plain content]) of
        Left  _ -> mempty -- Should be unreachable.
        Right t -> t

--------------------------------------------------------------------------------
listHeaders :: (HeaderInfo [Inline]  -> ListAttributes)
            -> [HeaderTree [Inline]] -> Block
listHeaders lattrs hts = go hts
  where
    go :: [HeaderTree [Inline]] -> Block
    go hts' = OrderedList (mklattrs hts') (map items hts')

    mklattrs :: [HeaderTree [Inline]] -> ListAttributes
    mklattrs [] = (1, DefaultStyle, DefaultDelim)
    mklattrs ((HeaderTree info _):_) = lattrs info

    items :: HeaderTree [Inline] -> [Block]
    items (HeaderTree info children) = [ Plain (title info)
                                       , listHeaders lattrs children
                                       ]

--------------------------------------------------------------------------------
headerTree :: [Block] -> [HeaderTree Text]
headerTree = map renderHeaders . headerTree' onlyHeaders

--------------------------------------------------------------------------------
headerTree' :: (Block -> [HeaderInfo a]) -> [Block] -> [HeaderTree a]
headerTree' f = go . map (\h -> HeaderTree h []) . query f

  where
    go :: [HeaderTree a] -> [HeaderTree a]
    go [] = []
    go (x:xs) = let (y,ys) = place x xs in y:go ys

    place :: HeaderTree a -> [HeaderTree a] -> (HeaderTree a, [HeaderTree a])
    place h [] = (h, [])
    place (HeaderTree h hs) (x@(HeaderTree h' _):xs)
      | level h' > level h = let (y,ys) = place x xs
                             in place (HeaderTree h (hs ++ [y])) ys
      | otherwise          = (HeaderTree h hs, x:xs)

--------------------------------------------------------------------------------
onlyHeaders :: Block -> [HeaderInfo [Inline]]
onlyHeaders (Header n a c) = [HeaderInfo n a c]
onlyHeaders _              = [ ]
