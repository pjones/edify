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
module Edify.ProjectTest
  ( main,
  )
where

import qualified Data.Yaml as YAML
import qualified Edify.Project as Project
import Test.Tasty
import Test.Tasty.HUnit

-- | Is the embedded default project configuration valid YAML?
testCanDecodeDefaultConfig :: Assertion
testCanDecodeDefaultConfig = do
  project <-
    YAML.decodeEither' Project.defaultProjectConfigBytes
      & either (assertFailure . show) pure
  Project.defaultProjectConfig @?= project

main :: IO TestTree
main =
  pure $
    testGroup
      "Project"
      [ testCase "Default config" testCanDecodeDefaultConfig
      ]
