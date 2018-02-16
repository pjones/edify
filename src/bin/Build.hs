{-# LANGUAGE RecordWildCards #-}

{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Build a complete Markdown project.
module Build
  ( Options
  , options
  , dispatch
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Options.Applicative
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName)

--------------------------------------------------------------------------------
-- Project Imports:
import qualified Text.Edify.Build.Markdown as Build
import qualified Text.Edify.Filter.Options as Filter

--------------------------------------------------------------------------------
data Options = Options
  { optionFilter :: Filter.Options
  , optionFile   :: FilePath -- FIXME: this is temporary.
  }

--------------------------------------------------------------------------------
options :: Parser Options
options = Options <$> Filter.options
                  <*> argument str (metavar "FILE")

--------------------------------------------------------------------------------
-- | Pass options on to the filters.
dispatch :: Options -> IO ()
dispatch Options{..} = do
  out <- (</> "build") <$> getCurrentDirectory
  createDirectoryIfMissing False out

  (doc, deps) <- Build.parse optionFilter optionFile
  Build.write doc (out </> takeFileName optionFile)
  print deps
