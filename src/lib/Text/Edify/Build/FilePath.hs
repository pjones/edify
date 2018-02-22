{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Utility functions for working with file names.
module Text.Edify.Build.FilePath
  ( findBaseDirectory
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Directory (getCurrentDirectory, doesPathExist)
import System.FilePath ((</>), takeDirectory)

--------------------------------------------------------------------------------
-- | Files and directories that mark the top of a project.
projectFiles :: [FilePath]
projectFiles =
  [ ".git"
  , "Makefile"
  , "GNUmakefile"
  ]

--------------------------------------------------------------------------------
-- | Find the base directory by walking up the file system and looking
-- for known files that indicate the top of a project.
findBaseDirectory :: (MonadIO m) => m FilePath
findBaseDirectory = liftIO getCurrentDirectory >>= go
  where
    go :: (MonadIO m) => FilePath -> m FilePath
    go "/" = fail "unable to find the top-level dir, use --top"
    go dir = do
      exist <- multiExist dir projectFiles
      if exist then return dir else go (takeDirectory dir)

--------------------------------------------------------------------------------
-- | Test any of the give files exist at the given path.
multiExist :: (MonadIO m) => FilePath -> [FilePath] -> m Bool
multiExist dir = go
  where
    go [] = return False
    go (x:xs) = do e <- liftIO (doesPathExist (dir </> x))
                   if e then return True else go xs
