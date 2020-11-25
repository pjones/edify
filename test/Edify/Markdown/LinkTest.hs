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
module Edify.Markdown.LinkTest
  ( main,
  )
where

import qualified Data.Text.Lazy.Builder as LTB
import Edify.Markdown.CommonTest
import Edify.Markdown.Link
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

linkTest :: Assertion
linkTest = do
  mapM_
    roundTrip
    [ ( "[AA](bar)",
        Link "AA" (Inline "bar" Nothing)
      ),
      ( "[BB](bar \"baz\")",
        Link "BB" (Inline "bar" $ Just "baz")
      ),
      ( "[CC](bar \"baz \\\"x\")",
        Link "CC" (Inline "bar" $ Just "baz \"x")
      ),
      ( "[DD][bar]",
        Link "DD" (Reference (Just "bar") (RefSep Nothing))
      ),
      ( "[EE][]",
        Link "EE" (Reference Nothing (RefSep Nothing))
      ),
      ( "[FF] [baz]",
        Link "FF" (Reference (Just "baz") (RefSep $ Just " "))
      ),
      ( "[GG]\n [baz]",
        Link "GG" (Reference (Just "baz") (RefSep $ Just "\n "))
      ),
      ( "[HH]\r\n [baz]",
        Link "HH" (Reference (Just "baz") (RefSep $ Just "\r\n "))
      ),
      ( "[II[foo]][baz]",
        Link "II[foo]" (Reference (Just "baz") (RefSep Nothing))
      ),
      ( "<bar>",
        AutoLink "bar"
      ),
      ( "<b<a>r>",
        AutoLink "b<a>r"
      )
    ]

  mapM_
    (\(input, expect) -> parseOnly linkP input >>= (@?= expect))
    [ ( "[01]",
        Link "01" (Reference Nothing (RefSep $ Just "\n"))
      ),
      ( "[02] \n [baz]",
        Link "02" (Reference (Just "baz") (RefSep $ Just "\n "))
      )
    ]

  mapM_
    ( \(input, leftovers, expect) ->
        parseWithLeftovers linkP input leftovers >>= (@?= expect)
    )
    [ ( "[A1]\n\n[bar]",
        "\n[bar]",
        Link "A1" (Reference Nothing (RefSep $ Just "\n"))
      )
    ]

  mapM_
    (parseShouldFail linkP)
    [ "[foo]: bar"
    ]
  where
    roundTrip :: (LText, Link Text) -> Assertion
    roundTrip (input, expect) = do
      actual <- parseOnly linkP input
      actual @?= expect
      LTB.toLazyText (linkT LTB.fromText expect) @?= input

defTest :: Assertion
defTest = do
  mapM_
    roundTrip
    [ ("[AA]: foobar\n", Definition "AA" "foobar" Nothing "\n"),
      ("[BB]: a \"b\"\n", Definition "BB" "a" (Just "b") "\n")
    ]

  mapM_
    (\(input, expect) -> parseOnly linkDefinitionP input >>= (@?= expect))
    [ -- Title given after newline:
      ("[01]: a \n \"b\"\n", Definition "01" "a" (Just "b") "\n")
    ]

  mapM_
    ( \(input, leftovers, expect) ->
        parseWithLeftovers linkDefinitionP input leftovers >>= (@?= expect)
    )
    [ -- Title given after newline must be indented:
      ("[A1]: foo\n\"bar\"", "\"bar\"", Definition "A1" "foo" Nothing "\n")
    ]
  where
    roundTrip :: (LText, Definition) -> Assertion
    roundTrip (input, expect) = do
      actual <- parseOnly linkDefinitionP input
      actual @?= expect
      LTB.toLazyText (linkDefinitionT expect) @?= input

main :: IO TestTree
main =
  pure $
    testGroup
      "Link"
      [ testCase "Parsing/Rendering" linkTest,
        testCase "Definition Parsing/Rendering" defTest
      ]
