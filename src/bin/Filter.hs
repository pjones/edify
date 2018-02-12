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
import Data.Monoid ((<>))
import Options.Applicative
import System.Directory (getCurrentDirectory)
import System.Exit (die)
import Text.Pandoc.JSON (toJSONFilter)

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Filter

--------------------------------------------------------------------------------
-- | Parse filter options.
options :: Parser Options
options = Options <$> many (strOption promoteCls)
                  <*> many (strOption removeCls)
  where
    promoteCls = long "promote" <> metavar "CLASS" <>
                 help "Remove a class name from all divs"

    removeCls  = long "remove" <> metavar "CLASS" <>
                 help "Remove divs with the given class"

--------------------------------------------------------------------------------
-- | Pass options on to the filters.
dispatch :: Options -> IO ()
dispatch opts = do
  pwd <- getCurrentDirectory

  toJSONFilter $ \p -> do
    fs  <- runFilterT pwd (filters opts p)
    case fs of
      Left e   -> die (show e)
      Right p' -> return p'
