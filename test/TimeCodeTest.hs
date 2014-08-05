{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
module TimeCodeTest (tests) where

--------------------------------------------------------------------------------
-- Library imports.
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.Hspec

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Time.TimeCode

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "TimeCode"
  [ testCase "asHHMMSS" hhmmssSpec
  , testCase "Parsing" parsedSpec
  ]

--------------------------------------------------------------------------------
hhmmssSpec ::Spec
hhmmssSpec = do
  it "1"    $ asHHMMSS (fromSeconds 1)    `shouldBe` "00:00:01"
  it "3600" $ asHHMMSS (fromSeconds 3600) `shouldBe` "01:00:00"
  it "3660" $ asHHMMSS (fromSeconds 3660) `shouldBe` "01:01:00"

--------------------------------------------------------------------------------
parsedSpec :: Spec
parsedSpec = do
    it "01:00:00" (go "01:00:00" `shouldReturn` 3600)
    it "01:01:00" (go "01:01:00" `shouldReturn` 3660)
    it "01:00:01" (go "01:00:01" `shouldReturn` 3601)
    it "aa:00:00" (go "aa:00:00" `shouldThrow`  anyIOException)
    it "00:aa:00" (go "00:aa:00" `shouldThrow`  anyIOException)
    it "00:00:aa" (go "00:00:aa" `shouldThrow`  anyIOException)
    it "00000000" (go "00000000" `shouldThrow`  anyIOException)
  where
    go :: Text -> IO Int
    go input = case parse input of
      Left e  -> fail (show e)
      Right t -> return (toSeconds t)
