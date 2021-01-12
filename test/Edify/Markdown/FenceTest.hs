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
import Data.List (isSuffixOf)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as LTB
import qualified Edify.Markdown.Attributes as Attrs
import Edify.Markdown.Common (wholelineP)
import Edify.Markdown.CommonTest (parseOnly)
import qualified Edify.Markdown.Fence as Fence
import qualified Edify.Text.Indent as Indent
import System.FilePath (takeFileName)
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
  files <-
    findByExtension [".md"] dataDir
      <&> ( filter
              ( \fname ->
                  let name = takeFileName fname
                   in "rewrite-" `isPrefixOf` name
                        && "0.md" `isSuffixOf` name
              )
              >>> map
                ( \name ->
                    ( name,
                      reverse name
                        & drop 4
                        & reverse
                        & (<> "1.md")
                    )
                )
          )
  key <- Attrs.toName "rewrite" & maybe (fail "impossible") pure
  pure $ testGroup "Rewrite" (map (go key) files)
  where
    ts :: Indent.Tabstop
    ts = Indent.defaultTabstop

    go :: Attrs.Name -> (FilePath, FilePath) -> TestTree
    go key (fileIn, fileCmp) =
      goldenVsString
        fileCmp
        fileCmp
        ( readFileLText fileIn
            >>= parseOnly (Fence.fenceP wholelineP)
            <&> ( Fence.rewrite ts LTB.fromText wholelineP (rewrite key)
                    >>> runIdentity
                    >>> either
                      ( show
                          >>> ("FAIL: " <>)
                          >>> (encodeUtf8 :: Text -> LByteString)
                      )
                      ( Fence.fenceT LTB.fromText
                          >>> LTB.toLazyText
                          >>> encodeUtf8
                      )
                )
        )

    rewrite ::
      Attrs.Name ->
      (Attrs.Attributes, Text) ->
      Identity Fence.Rewrite
    rewrite key (attrs, text)
      | attrs ^. #attrPairs . at key == Just "yes" =
        mempty
          & #rewriteAttrs ?~ (attrs & #attrPairs . at key ?~ "no")
          & #rewriteBody
            ?~ ( Text.replace "Hello" "Rewritten" text
                   & addFences attrs
               )
          & pure
      | otherwise =
        pure (Fence.Rewrite Nothing Nothing)

    addFences :: Attrs.Attributes -> Text -> Text
    addFences attrs body
      | Just key <- Attrs.toName "add-fences",
        Just "yes" <- attrs ^. #attrPairs . at key =
        "```\n" <> body <> "```\n"
      | otherwise = body
