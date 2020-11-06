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
module FenceTest
  ( main,
  )
where

import qualified Data.Attoparsec.Text.Lazy as Atto
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as LTB
import qualified Edify.Text.Fence as Fence
import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Test.Tasty.HUnit

main :: IO TestTree
main = goldenFenceTests

dataDir :: FilePath
dataDir = "test/data/fence"

parseOnly :: Atto.Parser a -> LText -> IO a
parseOnly parser input =
  case Atto.parse parser input of
    Atto.Fail _ _ err ->
      assertFailure (toString input <> ": " <> err)
    Atto.Done t actual
      | LText.null t ->
        pure actual
      | otherwise ->
        assertFailure (toString input <> ": left overs: " <> toString t)

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
            >>= parseOnly Fence.fenceP
              <&> ( Fence.fenceT
                      >>> LTB.toLazyText
                      >>> encodeUtf8
                  )
        )
