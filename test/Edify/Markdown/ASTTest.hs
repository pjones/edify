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
module Edify.Markdown.ASTTest
  ( main,
  )
where

import Control.Lens ((^..))
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.IO.Utf8 as Utf8
import qualified Edify.Markdown.AST as AST
import Edify.Markdown.CommonTest (parseOnly)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty.HUnit

main :: IO TestTree
main = do
  gs <- goldenAstTests
  pure $
    testGroup
      "AST"
      [ gs,
        testCase "Extract URLs" testExtractURLs
      ]

dataDir :: FilePath
dataDir = "test/data/ast"

goldenAstTests :: IO TestTree
goldenAstTests = do
  files <- findByExtension [".md"] dataDir
  pure $ testGroup "Decoding/Encoding" (map go files)
  where
    go :: FilePath -> TestTree
    go file =
      goldenVsString
        file
        file
        ( Utf8.readFile file
            >>= parseOnly AST.markdownP
              <&> ( AST.markdownT
                      >>> LTB.toLazyText
                      >>> encodeUtf8
                  )
        )

extractURLs :: FilePath -> IO [Text]
extractURLs file =
  Utf8.readFile (dataDir </> file)
    >>= parseOnly AST.markdownP
    <&> (^.. AST.blocks . AST.urls)

testExtractURLs :: Assertion
testExtractURLs = do
  as <- extractURLs "a.md"
  as @?= ["#foo", "#bar", "http://example.com"]
