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

import Control.Lens ((^.))
import Data.Functor.Foldable (cata)
import qualified Edify.Compiler.Audit as Audit
import qualified Edify.Compiler.Fingerprint as Fingerprint
import qualified Edify.Compiler.Options as Options

-- | Generate allow files for Markdown documents.
--
-- @since 0.5.0.0
allowMarkdown ::
  forall m.
  MonadIO m =>
  Options.Options ->
  NonEmpty FilePath ->
  m ()
allowMarkdown options files =
  Audit.audit options files >>= \case
    Left e -> die (show e) -- FIXME: proper error display
    Right audit -> do
      commands audit
        & fingerprint
        & Fingerprint.write
          ( options
              ^. #optionsUserConfig
                . #userCommandAllowDir
          )
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

-- | Generate fingerprint files for the given inputs.
--
-- @since 0.5.0.0
main ::
  MonadIO m =>
  -- | Compiler options.
  Options.Options ->
  -- | The files to fingerprint.
  NonEmpty FilePath ->
  -- | IO actions that create the files.
  m ()
main = allowMarkdown
