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
module Edify.Markdown.AttributesTest
  ( main,
  )
where

import Edify.Markdown.Attributes
import Edify.Markdown.CommonTest
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

testAttributesParsing :: Assertion
testAttributesParsing =
  mapM_
    go
    [ ( "{}",
        Attributes Nothing mempty mempty
      ),
      ( "{-}",
        Attributes Nothing (classes ["unnumbered"]) mempty
      ),
      ( "{#foo}",
        Attributes (idj "foo") mempty mempty
      ),
      ( "{#foo .bar .baz}",
        Attributes (idj "foo") (classes ["bar", "baz"]) mempty
      ),
      ( "{.bar .baz}",
        Attributes Nothing (classes ["bar", "baz"]) mempty
      ),
      ( "{.foo-\\000020}",
        Attributes Nothing (classes ["foo- "]) mempty
      ),
      ( "{.\\&}",
        Attributes Nothing (classes ["&"]) mempty
      ),
      ( "{key=value}",
        Attributes Nothing mempty (kvs [("key", "value")])
      ),
      ( "{x=y z=w}",
        Attributes Nothing mempty (kvs [("x", "y"), ("z", "w")])
      ),
      ( "{x=y z=w #foo .baz}",
        Attributes (idj "foo") (classes ["baz"]) (kvs [("x", "y"), ("z", "w")])
      ),
      ( "{x= y z=w\n#foo .baz}",
        Attributes (idj "foo") (classes ["baz"]) (kvs [("x", "y"), ("z", "w")])
      )
    ]
  where
    go :: (LText, Attributes) -> Assertion
    go (input, expected) = do
      actual <- parseOnly attributesP input
      actual @?= expected
    idj :: Text -> Maybe AttrName
    idj = mkAttrName >>> maybe (error "bad id") Just
    classes :: [Text] -> [CssIdent]
    classes = mapMaybe mkCssIdent
    kvs :: [(Text, Text)] -> [(AttrName, AttrValue)]
    kvs = mapMaybe (\(k, v) -> (,mkAttrValue v) <$> mkAttrName k)

testAttrValue :: Assertion
testAttrValue = do
  mapM_
    shouldPass
    [ ("this-is-a-test", "this-is-a-test", "this-is-a-test"),
      ("&amp;", "&", "&amp;"),
      ("A&amp;B", "A&B", "A&amp;B"),
      ("'this is a <test>'", "this is a <test>", "\"this is a <test>\""),
      ("\"this is a <test>\"", "this is a <test>", "\"this is a <test>\""),
      ("another&#x0a;test", "another\ntest", "\"another&#xa;test\""),
      ("another&#xa;test", "another\ntest", "\"another&#xa;test\"")
    ]
  mapM_
    shouldFail
    [ "this is a test",
      "A&W",
      "'A&W'"
    ]
  where
    shouldPass :: (LText, Text, LText) -> Assertion
    shouldPass (input, decodeExpect, encodeExpect) = do
      actual <- parseOnly attributeValueP input
      getAttrValue actual @?= decodeExpect
      attributeValueT (mkAttrValue decodeExpect) @?= encodeExpect
    shouldFail :: LText -> Assertion
    shouldFail = parseShouldFail attributeValueP

testCssIdent :: Assertion
testCssIdent = do
  mapM_
    shouldPass
    [ ("foo", "foo", "foo"),
      ("\\&", "&", "\\000026"),
      ("-this-is-a-\\26test", "-this-is-a-&test", "-this-is-a-\\000026test"),
      ("-this-is-a-\\000026test", "-this-is-a-&test", "-this-is-a-\\000026test"),
      ("-this-is-a-\\26 test", "-this-is-a-&test", "-this-is-a-\\000026test"),
      ("-in-the-\\&-middle", "-in-the-&-middle", "-in-the-\\000026-middle"),
      ("-in-the-\\26-middle", "-in-the-&-middle", "-in-the-\\000026-middle"),
      ("-in-the-\\26 -middle", "-in-the-&-middle", "-in-the-\\000026-middle"),
      ("-in-the-\\000026-middle", "-in-the-&-middle", "-in-the-\\000026-middle")
    ]
  mapM_
    shouldFail
    [ "foo bar",
      "&bar"
    ]
  where
    shouldPass :: (LText, Text, LText) -> Assertion
    shouldPass (input, decodeExpect, encodeExpect) = do
      decodeActual <- parseOnly cssIdentifierP input
      getCssIdent decodeActual @?= decodeExpect
      cssIdentifierT decodeActual @?= encodeExpect
    shouldFail :: LText -> Assertion
    shouldFail = parseShouldFail cssIdentifierP

main :: IO TestTree
main =
  pure $
    testGroup
      "Attributes"
      [ testCase "Parsing" testAttributesParsing,
        testCase "Values" testAttrValue,
        testCase "Classes" testCssIdent
      ]
