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
import qualified Data.Text.Lazy.IO.Utf8 as Utf8
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
        ( Utf8.readFile file
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
  pure $ testGroup "Rewrite" (map go files)
  where
    ts :: Indent.Tabstop
    ts = Indent.defaultTabstop

    go :: (FilePath, FilePath) -> TestTree
    go (fileIn, fileCmp) =
      goldenVsString
        fileCmp
        fileCmp
        ( Utf8.readFile fileIn
            >>= parseOnly (Fence.fenceP wholelineP)
            <&> ( Fence.rewrite ts LTB.fromText wholelineP rewrite
                    >>> runIdentity
                    >>> either
                      ( show
                          >>> ("FAIL: " <>)
                          >>> (encodeUtf8 :: Text -> LByteString)
                      )
                      ( maybe
                          mempty
                          ( Fence.fenceT LTB.fromText
                              >>> LTB.toLazyText
                              >>> encodeUtf8
                          )
                      )
                )
        )

    rewrite ::
      (Attrs.Attributes, Text) ->
      Identity Fence.Rewrite
    rewrite (attrs, text)
      | attrs ^. Attrs.at "rewrite" == Just "yes" =
        pure
          Fence.Rewrite
            { rewriteAttrs = Just (attrs & Attrs.at "rewrite" ?~ "no"),
              rewriteBody =
                Just
                  ( Text.replace "Hello" "Rewritten" text
                      & addFences attrs
                  )
            }
      | attrs ^. Attrs.at "discard" == Just "yes" =
        pure Fence.Discard
      | otherwise =
        pure (Fence.Rewrite Nothing Nothing)

    addFences :: Attrs.Attributes -> Text -> Text
    addFences attrs body
      | Just key <- Attrs.toName "add-fences",
        Just "yes" <- attrs ^. #attrPairs . at key =
        "```\n" <> body <> "```\n"
      | otherwise = body
