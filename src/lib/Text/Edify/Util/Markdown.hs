{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Helper functions for using Pandoc to parse markdown files.
module Text.Edify.Util.Markdown
  ( readerOptions
  , parseMarkdown
  ) where

--------------------------------------------------------------------------------
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (def)
import qualified Data.Set as Set
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Readers.Markdown (readMarkdown)

import Text.Pandoc.Options ( ReaderOptions(..)
                           , pandocExtensions
                           , githubMarkdownExtensions
                           )

--------------------------------------------------------------------------------
readerOptions :: ReaderOptions
readerOptions =
  def { readerSmart      = True
      , readerStandalone = True
      , readerExtensions = Set.unions [ pandocExtensions
                                      , githubMarkdownExtensions
                                      ]
      }

--------------------------------------------------------------------------------
parseMarkdown :: (MonadIO m) => FilePath -> m (Either String Pandoc)
parseMarkdown path = do
  str <- liftIO (readFile path)

  case readMarkdown readerOptions str of
    Left e  -> return (Left $ show e)
    Right p -> return (Right p)
