{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
module Text.Edify.Build.Markdown
  ( parse
  , write
  ) where

--------------------------------------------------------------------------------
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class (MonadIO)
import Text.Pandoc.Definition (Pandoc)

--------------------------------------------------------------------------------
import qualified Text.Edify.Filter as Filter
import qualified Text.Edify.Filter.FilterT as Filter
import qualified Text.Edify.Util.Markdown as Markdown

--------------------------------------------------------------------------------
parse :: (MonadIO m, MonadFail m)
      => Filter.Options
      -> FilePath
      -> m (Pandoc, [FilePath])
parse opts file = do
  result <- Filter.runFilterT (Just file) (Filter.filters opts) go
  either Fail.fail return result

  where
    go = do
      doc  <- Filter.processFile file
      deps <- Filter.getDependencies
      -- FIXME: find all image deps too
      return (doc, deps)

--------------------------------------------------------------------------------
write :: (MonadIO m) => Pandoc -> FilePath -> m ()
write = Markdown.writeMarkdownFile
