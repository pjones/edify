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
module Edify.Compiler.Error
  ( Error (..),
    renderError,
  )
where

import qualified Edify.Format as Format
import qualified Edify.Input as Input
import qualified Edify.Markdown.Fence as Fence

-- | Errors that might occur during a rewrite.
--
-- @since 0.5.0.0
data Error
  = -- | Errors from the "Input" module.
    InputError !Input.Input !Input.Error
  | -- | A cycle was found while recording a dependency.
    DependencyCycleError !FilePath !FilePath ![FilePath]
  | -- | Errors from the "Format" module.
    FormatError !Input.Input !Format.Error
  | -- | Failure to parse a markdown file.
    ParseError !Input.Input ![String] !String
  | -- | Failed to parse the output of a div rewrite.
    DivRewriteError !Input.Input !Fence.RewriteError
  deriving stock (Generic, Show)

-- | FIXME: Write description for renderError
--
-- @since 0.5.0.0
renderError :: Error -> String
renderError = show -- FIXME: pretty print this!
