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
        Link "DD" (Reference (RefSep Nothing) (RefText "bar"))
      ),
      ( "[EE][]",
        Link "EE" (Reference (RefSep Nothing) NoRefText)
      ),
      ( "[FF] [baz]",
        Link "FF" (Reference (RefSep $ Just " ") (RefText "baz"))
      ),
      ( "[GG]\n [baz]",
        Link "GG" (Reference (RefSep $ Just "\n ") (RefText "baz"))
      ),
      ( "[HH]\r\n [baz]",
        Link "HH" (Reference (RefSep $ Just "\r\n ") (RefText "baz"))
      ),
      ( "[II[foo]][baz]",
        Link "II[foo]" (Reference (RefSep Nothing) (RefText "baz"))
      ),
      ( "<http://bar>",
        AutoLink "http://bar"
      )
    ]

  mapM_
    (\(input, expect) -> parseOnly linkP input >>= (@?= expect))
    [ ( "[01]",
        Link "01" (Reference (RefSep $ Just "\n") ShortcutRef)
      ),
      ( "[02] \n [baz]",
        Link "02" (Reference (RefSep $ Just " \n ") (RefText "baz"))
      )
    ]

  mapM_
    ( \(input, leftovers, expect) ->
        parseWithLeftovers linkP input leftovers >>= (@?= expect)
    )
    [ ( "[A1]\n\n[bar]",
        "\n[bar]",
        Link "A1" (Reference (RefSep $ Just "\n") ShortcutRef)
      ),
      ( "[A2] foo",
        "foo",
        Link "A2" (Reference (RefSep $ Just " ") ShortcutRef)
      )
    ]

  mapM_
    (parseShouldFail linkP)
    [ "[foo]: bar",
      "[foo]{"
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
