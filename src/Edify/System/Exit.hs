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
    catchSync,
  )
where

import qualified Control.Exception as Exception
import qualified Edify.Text.Pretty as P
import qualified System.Exit as Exit

-- | Write the given error message to 'stderr' and exit with a failure
-- code.
--
-- @since 0.5.0.0
withError :: MonadIO m => P.Doc P.AnsiStyle -> m a
withError doc = do
  let msg = mconcat [P.red "ERROR: ", doc, P.hardline]
  liftIO (P.hPutDoc stderr msg)
  exitFailure

-- | If the given action throws a synchronous exception then exit the
-- current process with an error message.
--
-- @since 0.5.0.0
catchSync :: IO a -> IO a
catchSync = (`Exception.catches` handlers)
  where
    handlers :: [Exception.Handler a]
    handlers =
      [ Exception.Handler async,
        Exception.Handler exit,
        Exception.Handler others
      ]

    -- Ignore these.
    async :: Exception.SomeAsyncException -> IO a
    async = Exception.throwIO

    -- Ignore these.
    exit :: Exit.ExitCode -> IO a
    exit = Exception.throwIO

    -- Report and die.
    others :: SomeException -> IO a
    others e =
      withError $
        P.vcat
          [ P.reflow "a system error slipped through the cracks:",
            P.callout (P.red (P.pretty (displayException e)))
          ]
