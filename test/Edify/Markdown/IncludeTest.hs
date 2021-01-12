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
module Edify.Markdown.IncludeTest
  ( main,
  )
where

import Data.Char (isSpace)
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as Builder
import Edify.Markdown.CommonTest
import Edify.Markdown.Include
import Edify.Text.Narrow (Token (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

includeTest :: Assertion
includeTest =
  mapM_
    shouldPass
    [ ("<<(foo.md)", Include "foo.md" Nothing "\n"),
      ("  <<(foo.md)", Include "foo.md" Nothing "\n"),
      ("<<(foo.md#bar)", Include "foo.md" (Just $ Token "bar") "\n"),
      ("<<( foo.md#bar )", Include "foo.md" (Just $ Token "bar") "\n"),
      ("<<(\n foo.md#bar )", Include "foo.md" (Just $ Token "bar") "\n")
    ]
  where
    shouldPass :: (LText, Include) -> Assertion
    shouldPass (input, expect) = do
      actual <- parseOnly includeP input
      actual @?= expect
      Builder.toLazyText (includeT expect) @?= normalize input

    normalize :: LText -> LText
    normalize = LText.filter (not . isSpace) >>> (<> "\n")

main :: IO TestTree
main =
  pure $
    testGroup
      "Include"
      [ testCase "Parsing/Rendering" includeTest
      ]
