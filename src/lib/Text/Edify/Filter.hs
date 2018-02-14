{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
module Text.Edify.Filter
  ( Options(..)
  , runFilters
  ) where

--------------------------------------------------------------------------------
-- Library imports.
import Control.Monad.IO.Class (MonadIO)
import Data.Bifunctor (bimap)
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.Walk

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Filter.Div (promoteDivByClass, removeDivByClass)
import Text.Edify.Filter.Exec (executeBlock)
import Text.Edify.Filter.FilterT (FilterT, runFilterT, processPandoc)
import Text.Edify.Filter.Insert (insertFile, insertParsedFile)

--------------------------------------------------------------------------------
data Options = Options
  { divClassesToPromote :: [String]
  , divClassesToRemove  :: [String]
  }

--------------------------------------------------------------------------------
runFilters :: (MonadIO m)
        => Options
        -> Pandoc
        -> m (Either String Pandoc)

runFilters opts doc = do
     result <- runFilterT Nothing fts (processPandoc doc)
     return (bimap show id result)

  where
    fts :: (MonadIO m) => [Pandoc -> FilterT m Pandoc]
    fts =
      [ bottomUpM insertParsedFile
      , walkM insertFile
      , walkM executeBlock
      , walkM (makeM (promoteDivByClass $ divClassesToPromote opts))
      , walkM (makeM (removeDivByClass  $ divClassesToRemove  opts))
      ]

    makeM :: (Monad m) => (Block -> Block) -> Block -> m Block
    makeM f b = return (f b)
