{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
-- Library imports.
import Test.Tasty

--------------------------------------------------------------------------------
-- Project imports.
import qualified ManifestTest
import qualified TimeCodeTest

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ ManifestTest.tests
  , TimeCodeTest.tests
  ]
