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
module Main
  ( main,
  )
where

import qualified CompilerTest
import qualified Edify.ProjectTest as ProjectTest
import qualified MarkdownTest
import qualified NarrowTest
import Test.Tasty

main :: IO ()
main = do
  tests <-
    sequence
      [ NarrowTest.main,
        MarkdownTest.main,
        CompilerTest.main,
        ProjectTest.main
      ]

  defaultMain (testGroup "Tests" tests)
