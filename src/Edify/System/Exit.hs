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
module Edify.System.Exit
  ( withError,
  )
where

import qualified Data.Text.Prettyprint.Doc as PP
import qualified Prettyprinter.Render.Terminal as PP

-- | Write the given error message to 'stderr' and exit with a failure
-- code.
--
-- @since 0.5.0.0
withError :: MonadIO m => PP.Doc PP.AnsiStyle -> m a
withError doc = do
  let msg =
        mconcat
          [ PP.annotate (PP.color PP.Red) "ERROR: ",
            doc,
            PP.hardline
          ]
  liftIO (PP.hPutDoc stderr msg)
  exitFailure
