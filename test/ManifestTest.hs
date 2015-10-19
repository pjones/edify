{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
module ManifestTest (tests) where

--------------------------------------------------------------------------------
-- Library imports.
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.File.Manifest

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Manifest"
  [ testCase "Parsing" parsedSpec
  ]

--------------------------------------------------------------------------------
parsedSpec :: Assertion
parsedSpec = do parsed <- go
                parsed @=? fileList
  where
    go :: IO (Either String [Text])
    go = convert <$> parseFile "test/manifest.txt"

    convert :: Either String Manifest -> Either String [Text]
    convert = fmap files

    fileList :: Either String [Text]
    fileList = Right [ "file1"
                     , "file2"
                     , "file3"
                     , "file4"
                     , "file5/a/b.md"
                     ]
