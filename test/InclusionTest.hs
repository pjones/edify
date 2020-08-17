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
module InclusionTest
  ( main,
  )
where

import qualified Edify.Input as Input
import qualified Edify.Project.Source as Source
import qualified Edify.Text.Inclusion as Inclusion
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty.HUnit

main :: IO TestTree
main = do
  chunking <- chunkingTests
  pure $
    testGroup
      "InclusionTest"
      [ chunking,
        testCase "Cycle Detection" cycleDetectionTest
      ]

dataDir :: FilePath
dataDir = "test/data/inclusion"

chunkingTests :: IO TestTree
chunkingTests = do
  files <- findByExtension [".md"] dataDir
  pure $ testGroup "Chunking" (map go files)
  where
    go :: FilePath -> TestTree
    go file =
      readFileText file
        <&> ( Inclusion.toChunks
                >>> either show (Inclusion.fromChunks >>> encodeUtf8)
            )
        & goldenVsString file file

cycleDetectionTest :: Assertion
cycleDetectionTest = do
  mapM_
    shouldPass
    [ "a.md",
      "b.md",
      "f.md"
    ]
  mapM_
    shouldFail
    [ "c.md",
      "d.md",
      "e.md"
    ]
  where
    load :: FilePath -> IO (Either Source.Error ([Source.Source], Source.Context))
    load file = Source.toSource (Input.filePathToInput (Just $ dataDir </> file))
    shouldPass :: FilePath -> Assertion
    shouldPass = load >=> either (show >>> assertFailure) (const pass)
    shouldFail :: FilePath -> Assertion
    shouldFail file =
      load file
        >>= either
          (const pass)
          (const $ assertFailure $ "should have failed: " <> file)
