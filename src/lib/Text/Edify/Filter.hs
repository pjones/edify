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
  ( FilterT
  , Options(..)
  , runFilterT
  , filters
  ) where

--------------------------------------------------------------------------------
-- Library imports.
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO)
import Text.Pandoc

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Filter.Div (promoteDivByClass, removeDivByClass)
import Text.Edify.Filter.Exec (executeBlock)
import Text.Edify.Filter.FilterT (FilterT, runFilterT)
import Text.Edify.Filter.Insert (insertFile)

--------------------------------------------------------------------------------
data Options = Options
  { divClassesToPromote :: [String]
  , divClassesToRemove  :: [String]
  }

--------------------------------------------------------------------------------
filters :: (MonadIO m)
        => Options
        -> Pandoc
        -> FilterT m Pandoc

filters opts p =
    foldM (flip ($)) p fts

  where
    fts :: (MonadIO m) => [Pandoc -> FilterT m Pandoc]
    fts =
      [ bottomUpM insertFile
      , bottomUpM executeBlock
      , bottomUpM (makeM (promoteDivByClass $ divClassesToPromote opts))
      , bottomUpM (makeM (removeDivByClass  $ divClassesToRemove  opts))
      ]

    makeM :: (Monad m) => (Block -> Block) -> Block -> m Block
    makeM f b = return (f b)
