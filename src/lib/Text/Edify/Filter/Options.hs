{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Options that control filters.
module Text.Edify.Filter.Options
  ( Options(..)
  , options
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Monoid ((<>))
import Options.Applicative

--------------------------------------------------------------------------------
data Options = Options
  { divClassesToPromote :: [String]
  , divClassesToRemove  :: [String]
  , outputVerbose       :: Bool
  }

--------------------------------------------------------------------------------
-- | Parse filter options.
options :: Parser Options
options =
  Options <$> many (strOption promoteCls)
          <*> many (strOption removeCls)
          <*> switch verbose
  where
    promoteCls =
      long "promote" <> metavar "CLASS" <>
      help "Remove a class name from all divs"

    removeCls =
      long "remove" <> metavar "CLASS" <>
      help "Remove divs with the given class"

    verbose =
      long "verbose" <>
      help "Enable verbose output"
