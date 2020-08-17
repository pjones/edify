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

import qualified Edify.Format.Markdown as Markdown
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

testHeaderParsing :: Assertion
testHeaderParsing =
  mapM_
    shouldPass
    [ ("## Simple", 2, "Simple"),
      ("## #1", 2, "#1"),
      ("### It's #1 ###", 3, "It's #1"),
      ("# Attrs {#id}", 1, "Attrs"),
      ("# Attrs # {#id}", 1, "Attrs"),
      ("Simple\n===", 1, "Simple"),
      ("Attrs {#id}\n===", 1, "Attrs"),
      ("Attrs {#id} Mixed\n===", 1, "Attrs Mixed")
    ]
  where
    shouldPass :: (LText, Int, Text) -> Assertion
    shouldPass (input, level, content) = do
      heading <-
        case Markdown.toChunks input of
          Left e ->
            assertFailure (toString input <> ": " <> show e)
          Right cs ->
            case listToMaybe [h | Markdown.CHeading h <- cs] of
              Nothing -> assertFailure (toString input <> ": no headings found")
              Just h -> pure h
      Markdown.headingLevel heading @?= level
      Markdown.headingContent heading @?= content

main :: IO TestTree
main =
  pure $
    testGroup
      "Markdown"
      [ testCase "Headers" testHeaderParsing
      ]
