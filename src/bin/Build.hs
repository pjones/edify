{-# LANGUAGE RecordWildCards #-}

{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Build a complete Markdown project.
module Build
  ( Options
  , options
  , dispatch
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Development.Shake (ShakeOptions(..), shake, shakeOptions)
import qualified Development.Shake as Shake

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Edify.Build.Options (Options(..), options)
import qualified Text.Edify.Build.Plan as Build
import qualified Text.Edify.Build.Rules as Build
import Text.Edify.Build.Target

--------------------------------------------------------------------------------
-- | Pass options on to the filters.
dispatch :: Options -> IO ()
dispatch opts = do
  targets <- targetsFromOptions opts

  shake shakeOptions { shakeFiles = optionsOutputDirectory opts
                     , shakeVerbosity = Shake.Normal
                     } $
    Build.rules opts >> mapM_ (Build.plan opts) targets
