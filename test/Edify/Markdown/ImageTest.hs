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
module Edify.Markdown.ImageTest
  ( main,
  )
where

import qualified Data.Text.Lazy.Builder as LTB
import Edify.Markdown.CommonTest
import Edify.Markdown.Image
import Edify.Markdown.Link
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

imageTest :: Assertion
imageTest =
  mapM_
    roundTrip
    [ ("![aa](bar)", img "aa" inline "bar"),
      ("![bb][bar]", img "bb" ref "bar")
    ]
  where
    roundTrip :: (LText, Image) -> Assertion
    roundTrip (input, expect) = do
      actual <- parseOnly imageP input
      actual @?= expect
      LTB.toLazyText (imageT expect) @?= input

    img :: Text -> (Text -> Destination) -> Text -> Image
    img alt f src = Image alt (f src)

    inline :: Text -> Destination
    inline = (`Inline` Nothing)

    ref :: Text -> Destination
    ref = (`Reference` RefSep Nothing) . Just

main :: IO TestTree
main =
  pure $
    testGroup
      "Image"
      [ testCase "Parsing/Rendering" imageTest
      ]
