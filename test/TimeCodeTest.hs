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
module TimeCodeTest (tests) where

--------------------------------------------------------------------------------
-- Library imports.
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Time.TimeCode

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "TimeCode"
  [ testGroup "asHHMMSS" hhmmssTest
  , testGroup "Parsing" parsedTest
  ]

--------------------------------------------------------------------------------
hhmmssTest :: [TestTree]
hhmmssTest =
  [ testCase "1"    (asHHMMSS (fromSeconds 1)    @=? "00:00:01")
  , testCase "3600" (asHHMMSS (fromSeconds 3600) @=? "01:00:00")
  , testCase "3660" (asHHMMSS (fromSeconds 3660) @=? "01:01:00")
  ]

--------------------------------------------------------------------------------
parsedTest :: [TestTree]
parsedTest =
    [ testCase "01:00:00" (go "01:00:00" @=? Just 3600)
    , testCase "01:01:00" (go "01:01:00" @=? Just 3660)
    , testCase "01:00:01" (go "01:00:01" @=? Just 3601)
    , testCase "aa:00:00" (go "aa:00:00" @=? Nothing)
    , testCase "00:aa:00" (go "00:aa:00" @=? Nothing)
    , testCase "00:00:aa" (go "00:00:aa" @=? Nothing)
    , testCase "00000000" (go "00000000" @=? Nothing)
    ]
  where
    go :: Text -> Maybe Int
    go input = case parse input of
      Left _  -> Nothing
      Right t -> Just (toSeconds t)
