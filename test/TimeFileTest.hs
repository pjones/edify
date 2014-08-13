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
import           Data.Map (Map)
import qualified Data.Map as M
import           Test.Tasty
import           Test.Tasty.Hspec

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
parsedSpec :: Spec
parsedSpec = it "Test file" (go `shouldReturn` timeEntries)
  where
    go :: IO (Either String (Map String TimeCode))
    go = parseFile "test/timecodes.txt"

    timeEntries :: Either String (Map String TimeCode)
    timeEntries = Right . M.fromList $
      [ ("foo-bar", fromSeconds 60)
      , ("baz",     fromSeconds 120)
      ]
