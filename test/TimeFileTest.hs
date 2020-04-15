{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
module TimeFileTest (tests) where


--------------------------------------------------------------------------------
-- Library imports.
import qualified Data.Map as M
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.File.Time
import Text.Edify.Time.TimeCode

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "TimeFile"
  [ testCase "Parsing" parsedSpec
  ]

--------------------------------------------------------------------------------
parsedSpec :: Assertion
parsedSpec = do actual <- go
                actual @?= timeEntries
  where
    go :: IO (Either Text (Map Text TimeCode))
    go = parseFile "test/timecodes.txt"

    timeEntries :: Either Text (Map Text TimeCode)
    timeEntries = Right . M.fromList $
      [ ("foo-bar", fromSeconds 60)
      , ("baz",     fromSeconds 120)
      ]
