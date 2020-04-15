{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Simple utility functions for parsing/generating Markdown via Pandoc.
module Text.Edify.Build.Markdown
  ( parse
  , write
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Data.Default (def)
import Text.Pandoc.Definition (Pandoc)

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Edify.Build.Target (Target(..), filterEnvFromTarget)
import Text.Edify.Build.Template (TemplateStyle(..))
import qualified Text.Edify.Filter.FilterT as Filter
import qualified Text.Edify.Rewrite.Outline as Rewrite
import qualified Text.Edify.Util.Markdown as Markdown

--------------------------------------------------------------------------------
-- | Parse a Markdown file and process it through the Edify filter
-- system.  Information about which file to read and how to process it
-- are taken from the 'Target' object.
--
-- Returns the 'Pandoc' object and a list files that the Markdown file
-- depends on.
parse :: (MonadIO m, MonadFail m) => Target -> m (Pandoc, [FilePath])
parse target@Target{..} = do
  result <- Filter.runFilterT (Just targetInputFile) env go
  either (fail . toString) return result

  where
    env :: (MonadIO m) => Filter.Env m
    env = filterEnvFromTarget target

    go :: (MonadIO m) => Filter.FilterT m (Pandoc, [FilePath])
    go = do
      doc  <- Filter.processFile targetInputFile
      deps <- Filter.getDependencies

      case targetTemplateStyle of
        Handout -> return (doc, deps)
        Slides  -> return (doc, deps)
        Outline -> return (Rewrite.outline def doc, deps)

--------------------------------------------------------------------------------
-- | Generate a Markdown file from a 'Pandoc' object.
write :: (MonadIO m) => Pandoc -> FilePath -> m (Maybe Text)
write = Markdown.writeMarkdownFile
