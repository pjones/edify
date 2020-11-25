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
module MarkdownTest
  ( main,
  )
where

import qualified Edify.Markdown.ASTTest as ASTTest
import qualified Edify.Markdown.AttributesTest as AttributesTest
import qualified Edify.Markdown.FenceTest as FenceTest
import qualified Edify.Markdown.HeadingTest as HeadingTest
import qualified Edify.Markdown.ImageTest as ImageTest
import qualified Edify.Markdown.LinkTest as LinkTest
import Test.Tasty (TestTree, testGroup)

main :: IO TestTree
main =
  testGroup "Markdown"
    <$> sequence
      [ AttributesTest.main,
        FenceTest.main,
        LinkTest.main,
        ImageTest.main,
        HeadingTest.main,
        ASTTest.main
      ]
