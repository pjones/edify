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
module Edify.Compiler.CycleTest
  ( main,
  )
where

import Edify.Compiler.Cycle
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

cycleTest :: Assertion
cycleTest = do
  mapM_
    cycles
    [ [('a', 'a')],
      [('c', 'd'), ('d', 'c')],
      [('e', 'f'), ('f', 'g'), ('f', 'h'), ('g', 'h'), ('h', 'f')]
    ]

  mapM_
    nocycles
    [ [('m', 'n')],
      [('o', 'p'), ('o', 'q'), ('q', 'p')]
    ]
  where
    check :: [(Char, Char)] -> (Cycles Char, Deps Char)
    check =
      foldl'
        ( \case
            p@(Cycles {}, _) -> const p
            (NoCycles, deps) -> \(x, y) -> depends x y deps
        )
        (NoCycles, emptyDeps)
    cycles :: [(Char, Char)] -> Assertion
    cycles ns = case check ns of
      (NoCycles, deps) ->
        assertFailure
          ( "expected cycle in: "
              <> show ns
              <> "\n"
              <> render deps
          )
      (Cycles _, _) -> pass
    nocycles :: [(Char, Char)] -> Assertion
    nocycles ns = case check ns of
      (NoCycles, _) -> pass
      (Cycles cs, deps) ->
        assertFailure
          ( "unexpected cycle in "
              <> show ns
              <> " where cycle is "
              <> show cs
              <> "\n"
              <> render deps
          )

main :: IO TestTree
main =
  pure $
    testGroup
      "Cycle"
      [ testCase "Cycle detection" cycleTest
      ]
