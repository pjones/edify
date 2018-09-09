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
module InclusionTest (tests) where

--------------------------------------------------------------------------------
-- Library imports.
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Build.Template (OutputFormat(..))
import qualified Text.Edify.Filter as Filter
import Text.Edify.Filter.FilterT (FilterT)
import qualified Text.Edify.Filter.FilterT as FilterT
import qualified Text.Edify.Filter.Options as FilterT
import Text.Edify.Util.Inclusion

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Inclusion"
  [ testCase "Parsing" parsedSpec
  , testCase "Cycles"  cycleSpec
  ]

--------------------------------------------------------------------------------
parsedSpec :: Assertion
parsedSpec = do
    forM_ inputs $ \(s, r) -> inclusionMarker s @?= r

  where
    inputs :: [(String, Maybe FileRef)]
    inputs =
      [ ("foobar",         Nothing)
      , ("<<foo.md",       Nothing)
      , ("<<(foo.md)",     Just (FileRef "foo.md" Nothing))
      , ("\n<<(foo.md)\n", Just (FileRef "foo.md" Nothing))
      , ("<<(foo.md#abc)", Just (FileRef "foo.md" (Just "abc")))
      ]

--------------------------------------------------------------------------------
cycleSpec :: Assertion
cycleSpec = do
    x <- hasCycle importWithoutCycle
    y <- hasCycle importWithCycle

    assertBool "no cycles expected" (not x)
    assertBool "expected a cycle"   y
  where
    hasCycle :: (MonadIO m) => FilterT m () -> m Bool
    hasCycle f = do
      result <- FilterT.runFilterT (Just "test/data/a.md") env f
      case result of
        Left  e -> liftIO (putStrLn e) >> return True
        Right _ -> return False

    env :: (MonadIO m) => FilterT.Env m
    env  = FilterT.Env { FilterT.envFilters          = Filter.filters opts
                       , FilterT.envOptions          = opts
                       , FilterT.envFormat           = Markdown
                       , FilterT.envOutputDirectory  = Just "test"
                       , FilterT.envProjectDirectory = Just "."
                       }

    opts :: FilterT.Options
    opts  = FilterT.Options { FilterT.divClassesToPromote = []
                            , FilterT.divClassesToRemove  = []
                            , FilterT.outputVerbose       = False
                            }

--------------------------------------------------------------------------------
importWithoutCycle :: (MonadIO m) => FilterT m ()
importWithoutCycle = FilterT.processFile "test/data/a.md" >> return ()

--------------------------------------------------------------------------------
importWithCycle :: (MonadIO m) => FilterT m ()
importWithCycle = FilterT.processFile "test/data/e.md" >> return ()
