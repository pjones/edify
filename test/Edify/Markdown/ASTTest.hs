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

import qualified Data.Text.Lazy.Builder as LTB
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
        ( readFileLText file
            >>= parseOnly AST.markdownP
              <&> ( AST.markdownT
                      >>> LTB.toLazyText
                      >>> encodeUtf8
                  )
        )

extractURLs :: FilePath -> IO [Text]
extractURLs file =
  readFileLText (dataDir </> file)
    >>= parseOnly AST.markdownP
    <&> AST.extractURLs

testExtractURLs :: Assertion
testExtractURLs = do
  as <- extractURLs "a.md"
  as @?= ["#foo", "#bar", "http://example.com"]
