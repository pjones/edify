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
module CompilerTest
  ( main,
  )
where

import qualified Edify.Compiler.CycleTest as CycleTest
import Test.Tasty (TestTree, testGroup)

main :: IO TestTree
main =
  testGroup "Compiler"
    <$> sequence
      [ CycleTest.main
      ]
