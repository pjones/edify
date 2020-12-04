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

import Control.Lens (at, (?~), (^.))
import Data.Generics.Labels ()
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as LTB
import qualified Edify.Markdown.Attributes as Attrs
import Edify.Markdown.Common (wholelineP)
import Edify.Markdown.CommonTest (parseOnly)
import qualified Edify.Markdown.Fence as Fence
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsString)

main :: IO TestTree
main = do
  gs <- goldenFenceTests
  rw <- testRewrite
  pure $
    testGroup
      "Fenced Blocks"
      [ gs,
        rw
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

testRewrite :: IO TestTree
testRewrite = do
  let fileIn = dataDir </> "rewrite-a0.md"
      fileCmp = dataDir </> "rewrite-a1.md"
  key <- Attrs.mkAttrName "rewrite" & maybe (fail "impossible") pure
  pure $
    goldenVsString
      "Rewrite"
      fileCmp
      ( readFileLText fileIn
          >>= parseOnly (Fence.fenceP wholelineP)
          <&> ( Fence.rewrite LTB.fromText wholelineP (rewrite key)
                  >>> runIdentity
                  >>> either
                    encodeUtf8
                    ( Fence.fenceT LTB.fromText
                        >>> LTB.toLazyText
                        >>> encodeUtf8
                    )
              )
      )
  where
    rewrite ::
      Attrs.AttrName ->
      (Attrs.Attributes, Text) ->
      Identity Fence.Rewrite
    rewrite key (attrs, text)
      | attrs ^. #attrPairs . at key == Just "yes" =
        pure $
          Fence.Rewrite
            (Just (attrs & #attrPairs . at key ?~ "no"))
            (Just (Text.replace "Hello" "Rewritten" text))
      | otherwise =
        pure (Fence.Rewrite Nothing Nothing)
