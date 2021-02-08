-- |
--
-- Copyright:
--   This file is part of the package Edify. It is subject to the
--   license terms in the LICENSE file found in the top-level
--   directory of this distribution and at:
--
--     https://github.com/pjones/edify
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the terms
--   contained in the LICENSE file.
--
-- License: Apache-2.0
--
-- Record fingerprint files for authorized actions.
module Edify.Compiler.Allow
  ( main,
  )
where

import Data.Foldable (foldrM)
import Data.Functor.Foldable (cata)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Edify.Compiler.Audit as Audit
import qualified Edify.Compiler.FilePath as FilePath
import qualified Edify.Compiler.Fingerprint as Fingerprint
import qualified Edify.Project as Project

-- | Generate allow files for Markdown documents.
--
-- @since 0.5.0.0
allowMarkdown ::
  forall m.
  MonadIO m =>
  -- | Directory where allow files can be written.
  FilePath ->
  -- | List of files to allow.
  NonEmpty FilePath ->
  -- | Allow action.
  m ()
allowMarkdown dir files =
  Audit.audit dir files >>= \case
    Left e -> die (show e) -- FIXME: proper error display
    Right audit -> do
      commands audit
        & fingerprint
        & Fingerprint.write dir
  where
    commands :: Audit.Audit -> [(FilePath, Text)]
    commands = cata $ \case
      Audit.AuditEnd -> mempty
      Audit.AuditItems items -> fold items
      Audit.AuditAsset _file -> mempty
      Audit.AuditFile _file items -> items
      Audit.AuditCommand path cmd _fp -> one (path, cmd)

    fingerprint :: [(FilePath, Text)] -> Fingerprint.Cache Fingerprint.Commands
    fingerprint = sortNub >>> foldr (uncurry Fingerprint.cache) mempty

-- | Generate allow files for JSON/YAML configuration files.
--
-- @since 0.5.0.0
allowConfig ::
  forall m.
  MonadIO m =>
  -- | Directory where allow files can be written.
  FilePath ->
  -- | List of files to allow.
  NonEmpty FilePath ->
  -- | Allow action.
  m ()
allowConfig dir files = do
  cache <- foldrM fingerprint mempty files
  Fingerprint.write dir cache
  where
    fingerprint ::
      FilePath ->
      Fingerprint.Cache Fingerprint.Self ->
      m (Fingerprint.Cache Fingerprint.Self)
    fingerprint file cache = do
      cache' <- Fingerprint.self file
      pure (cache <> cache')

-- | Generate fingerprint files for the given inputs.
--
-- @since 0.5.0.0
main ::
  MonadIO m =>
  -- | Directory where allow files can be written.
  FilePath ->
  -- | The files to fingerprint.
  NonEmpty FilePath ->
  -- | IO actions that create the files.
  m ()
main dir files = do
  let (configs, markdowns) = NonEmpty.partition isConfigFile files
  maybe pass (allowConfig dir) $ nonEmpty configs
  maybe pass (allowMarkdown dir) $ nonEmpty markdowns
  where
    isConfigFile :: FilePath -> Bool
    isConfigFile =
      FilePath.takeFileName
        >>> (`elem` Project.projectConfigFiles)
