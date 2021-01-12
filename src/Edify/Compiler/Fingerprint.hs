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
module Edify.Compiler.Fingerprint
  ( Fingerprints,
    fingerprint,
  )
where

import Control.Lens ((^..))
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Edify.Compiler.Lang (Command)
import qualified Edify.Markdown.AST as AST
import qualified Edify.Markdown.Attributes as Attrs
import qualified Edify.Markdown.Fence as Fence

-- | FIXME: Write documentation for Fingerprints
--
-- @since 0.5.0.0
type Fingerprints = Vector (Command, Text)

-- | Generate a fingerprint for the given AST block.
--
-- @since 0.5.0.0
fingerprint :: AST.Block -> Fingerprints
fingerprint src =
  Vector.fromList $
    map (\cmd -> (cmd, hexsum cmd)) $
      commands src
  where
    commands :: AST.Block -> [Command]
    commands block =
      block ^.. AST.fences . Fence.attrs . Attrs.at "exec"
        & catMaybes

    hexsum :: Text -> Text
    hexsum =
      encodeUtf8
        >>> SHA256.hash
        >>> Base16.encode
        >>> decodeUtf8
