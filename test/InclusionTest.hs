{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
module InclusionTest (tests) where

--------------------------------------------------------------------------------
-- Library imports.
import Control.Monad (forM_)
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Util.Inclusion

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Inclusion"
  [ testCase "Parsing" parsedSpec
  ]

--------------------------------------------------------------------------------
parsedSpec :: Assertion
parsedSpec = do
    forM_ inputs $ \(s, r) -> inclusionMarker s @?= r

  where
    inputs :: [(String, Maybe FilePath)]
    inputs =
      [ ("foobar",         Nothing)
      , ("<<foo.md",       Nothing)
      , ("<<(foo.md)",     Just "foo.md")
      , ("\n<<(foo.md)\n", Just "foo.md")
      ]
