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
module Edify.Markdown.FenceTest
  ( main,
  )
where

import qualified Data.Text.Lazy.Builder as LTB
import Edify.Markdown.Common (wholelineP)
import Edify.Markdown.CommonTest (parseOnly)
import qualified Edify.Markdown.Fence as Fence
import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsString)

main :: IO TestTree
main = do
  gs <- goldenFenceTests
  pure $
    testGroup
      "Fenced Blocks"
      [ gs
      ]

dataDir :: FilePath
dataDir = "test/data/fence"

goldenFenceTests :: IO TestTree
goldenFenceTests = do
  files <- findByExtension [".md"] dataDir
  pure $ testGroup "Decoding/Encoding" (map go files)
  where
    go :: FilePath -> TestTree
    go file =
      goldenVsString
        file
        file
        ( readFileLText file
            >>= parseOnly (Fence.fenceP wholelineP)
              <&> ( Fence.fenceT LTB.fromText
                      >>> LTB.toLazyText
                      >>> encodeUtf8
                  )
        )
