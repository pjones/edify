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
  , OutputFormat(..)
  , options
  , filters
  , runFilters
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad.IO.Class (MonadIO)
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.Walk

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Edify.Build.Template (OutputFormat(..))
import Text.Edify.Filter.Div (promoteDivByClass, removeDivByClass)
import Text.Edify.Filter.Exec (executeBlock)
import Text.Edify.Filter.FilterT (FilterT, Env(..), runFilterT, processPandoc)
import Text.Edify.Filter.Image (imageRewrite)
import Text.Edify.Filter.Insert (insertFile, insertParsedFile)
import Text.Edify.Filter.Options

--------------------------------------------------------------------------------
filters :: (MonadIO m) => Options -> [Pandoc -> FilterT m Pandoc]
filters opts =
    [ walkM insertFile
    , walkM executeBlock
    , walkM imageRewrite
    , bottomUpM insertParsedFile
    , walkM (makeM (promoteDivByClass $ divClassesToPromote opts))
    , walkM (makeM (removeDivByClass  $ divClassesToRemove  opts))
    ]

  where
    makeM :: (Monad m) => (Block -> Block) -> Block -> m Block
    makeM f b = return (f b)

--------------------------------------------------------------------------------
runFilters :: (MonadIO m)
        => Options
        -> Pandoc
        -> m (Either String Pandoc)

runFilters opts doc =
    runFilterT Nothing env (processPandoc doc)
  where
    env :: (MonadIO m) => Env m
    env =
      Env { envFilters = filters opts
          , envOptions = opts
          , envFormat  = Markdown
          , envOutputDirectory = Nothing
          , envProjectDirectory = Nothing
          }
