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
module NarrowTest
  ( main,
  )
where

import qualified Edify.Text.Indent as Indent
import qualified Edify.Text.Narrow as Narrow
import System.FilePath (dropExtension, takeBaseName)
import Test.Tasty
import Test.Tasty.Golden (findByExtension, goldenVsString)

main :: IO TestTree
main = do
  golden <- goldenTests
  pure $ testGroup "NarrowTest" [golden]

goldenTests :: IO TestTree
goldenTests = do
  files <- findByExtension [".golden"] "test/data/narrow"
  pure $ testGroup "Golden" (map go files)
  where
    go :: FilePath -> TestTree
    go golden =
      let file = dropExtension golden
          token = Narrow.Token (takeBaseName file & dropExtension & toText)
       in goldenVsString
            file
            golden
            ( readFileText file
                <&> ( Narrow.narrow token
                        >>> either show (Indent.unindent Indent.defaultTabstop)
                        >>> encodeUtf8
                    )
            )
