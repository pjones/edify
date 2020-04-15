{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Turn a Pandoc document to a list of headers.
module Text.Edify.Rewrite.Outline
  ( Config(..)
  , outline
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Default
import Text.Pandoc.Definition (Pandoc(..))
import qualified Text.Pandoc.Definition as Pandoc

--------------------------------------------------------------------------------
-- Edify Imports:
import Text.Edify.Util.HeaderTree

--------------------------------------------------------------------------------
-- | Configuration to control the outline process.
newtype Config = Config
  { outlineMaxLevel :: Int
  }

instance Default Config where
  def = Config { outlineMaxLevel = 2
               }

--------------------------------------------------------------------------------
-- | Rewrite a Pandoc document so all headings are turning into
-- ordered list, thus producing an outline of a document.
outline :: Config -> Pandoc -> Pandoc
outline Config{..} (Pandoc m bs) = Pandoc m [transform bs]
  where
    transform :: [Pandoc.Block] -> Pandoc.Block
    transform = listHeaders lattrs . foldr limit [] . headerTree' onlyHeaders

    lattrs :: HeaderInfo a -> Pandoc.ListAttributes
    lattrs info =
      case level info of
        1 -> (1, Pandoc.DefaultStyle, Pandoc.DefaultDelim)
        2 -> (1, Pandoc.LowerAlpha, Pandoc.OneParen)
        _ -> (1, Pandoc.LowerRoman, Pandoc.DefaultDelim)

    limit :: HeaderTree a -> [HeaderTree a] -> [HeaderTree a]
    limit (HeaderTree info cs) out =
      if level info > outlineMaxLevel || skipBasedOnClasses info
        then out
        else HeaderTree info (foldr limit [] cs) : out

    skipBasedOnClasses :: HeaderInfo a -> Bool
    skipBasedOnClasses info = "unnumbered" `elem` classes info

    classes :: HeaderInfo a -> [Text]
    classes hi = let (_, cs, _) = attrs hi in cs
