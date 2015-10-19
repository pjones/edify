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
import           Control.Monad (foldM)
import           Options.Applicative
import           Text.Pandoc
import           Text.Pandoc.JSON

--------------------------------------------------------------------------------
-- | Project imports.
import Text.Edify.Filter.Exec (executeBlock)
import Text.Edify.Filter.Insert (insertFile)
import Text.Edify.Filter.Div (promoteDivByClass, removeDivByClass)

--------------------------------------------------------------------------------
-- | No options yet.
data Options = Options
  { divClassesToPromote :: [String]
  , divClassesToRemove  :: [String]
  }

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
dispatch opts = toJSONFilter (\p -> foldM (flip ($)) p filters)
  where
    filters :: [Pandoc -> IO Pandoc]
    filters =
      [ bottomUpM insertFile
      , bottomUpM executeBlock
      , bottomUpM (makeM (promoteDivByClass $ divClassesToPromote opts))
      , bottomUpM (makeM (removeDivByClass  $ divClassesToRemove  opts))
      ]

    makeM :: (Block -> Block) -> Block -> IO Block
    makeM f b = return (f b)
