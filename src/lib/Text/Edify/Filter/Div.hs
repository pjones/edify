{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Alter @div@ notes in a @Pandoc@ value.
module Text.Edify.Filter.Div
       ( promoteDivByClass
       , removeDivByClass
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import Data.List
import Text.Pandoc

--------------------------------------------------------------------------------
-- | Promote a @div@ by removing a class name.  This is useful for
-- including slide notes in a handout by removing the @notes@ class.
promoteDivByClass :: [String] -> Block -> Block
promoteDivByClass cls (Div attr bs) = Div (foldr removeClass attr cls) bs
promoteDivByClass _ x               = x

--------------------------------------------------------------------------------
removeDivByClass :: [String] -> Block -> Block
removeDivByClass cls d@(Div attr _) = if any (attrHasClass attr) cls
                                        then Null else d
removeDivByClass _ x = x

--------------------------------------------------------------------------------
-- | Remove a class name from the given attributes.
removeClass :: String -> Attr -> Attr
removeClass cls (x, cs, y) = (x, delete cls cs, y)

--------------------------------------------------------------------------------
-- | Test to see if a attribute has a given class name set.
attrHasClass :: Attr -> String -> Bool
attrHasClass (_, cs, _) cls = cls `elem` cs
