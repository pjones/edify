{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Command to filter a Pandoc JSON stream.
module Filter
  ( Options
  , options
  , dispatch
  ) where

--------------------------------------------------------------------------------
-- Library imports.
import System.Exit (die)
import Text.Pandoc.JSON (toJSONFilter)

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Filter

--------------------------------------------------------------------------------
-- | Pass options on to the filters.
dispatch :: Options -> IO ()
dispatch opts = do
  let opts' = Options { divClassesToPromote = divClassesToPromote opts
                      , divClassesToRemove = divClassesToRemove opts
                      , outputVerbose = outputVerbose opts
                      }

  toJSONFilter $ \p -> do
    fs  <- runFilters opts' p
    case fs of
      Left e   -> die e
      Right p' -> return p'
