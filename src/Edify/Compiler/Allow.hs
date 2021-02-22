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

import Data.Functor.Foldable (cata)
import qualified Edify.Compiler.Audit as Audit
import qualified Edify.Compiler.Error as Error
import qualified Edify.Compiler.Fingerprint as Fingerprint
import qualified Edify.System.Exit as Exit
import qualified System.Directory as Directory

-- | Generate allow files for the given input files.
--
-- @since 0.5.0.0
allow :: forall m. MonadIO m => FilePath -> NonEmpty FilePath -> m ()
allow dir files = do
  cwd <- liftIO Directory.getCurrentDirectory
  Audit.audit dir files >>= \case
    Left e -> Exit.withError (Error.render' cwd dir files e)
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
    fingerprint =
      let gen = Fingerprint.generate . one
       in sortNub >>> foldr (uncurry Fingerprint.cache . second gen) mempty

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
main = allow
