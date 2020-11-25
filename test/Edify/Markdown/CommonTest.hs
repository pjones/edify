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
module Edify.Markdown.CommonTest
  ( parseOnly,
    parseWithLeftovers,
    parseShouldFail,
    parseShouldSucceed,
  )
where

import qualified Data.Attoparsec.Text.Lazy as Atto
import qualified Data.Text.Lazy as LText
import Test.Tasty.HUnit

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

parseWithLeftovers :: Atto.Parser a -> LText -> LText -> IO a
parseWithLeftovers parser input leftovers =
  case Atto.parse parser input of
    Atto.Fail _ _ err ->
      assertFailure (toString input <> ": " <> err)
    Atto.Done t actual
      | LText.null t ->
        assertFailure
          ( toString input
              <> ": should have had leftovers: "
              <> toString leftovers
          )
      | t == leftovers ->
        pure actual
      | otherwise ->
        assertFailure
          ( toString input
              <> ": wrong leftovers: "
              <> toString t
              <> " ≠ "
              <> toString leftovers
          )

parseShouldFail :: Atto.Parser a -> LText -> Assertion
parseShouldFail parser input =
  case Atto.parse (parser <* Atto.endOfInput) input of
    Atto.Fail {} ->
      pass
    Atto.Done {} ->
      assertFailure (toString input <> ": should have failed to parse")

parseShouldSucceed :: Atto.Parser a -> LText -> Assertion
parseShouldSucceed parser input =
  case Atto.parse (parser <* Atto.endOfInput) input of
    Atto.Fail {} ->
      assertFailure (toString input <> ": should have parsed correctly")
    Atto.Done {} ->
      pass
