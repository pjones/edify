{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Execute a script and insert the output into Pandoc.
module Text.Edify.Filter.Exec
       ( executeBlock
       ) where

--------------------------------------------------------------------------------
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Exit
import System.Process
import Text.Pandoc

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Filter.FilterT

--------------------------------------------------------------------------------
-- FIXME: change exec attribute after processing.
executeBlock :: (MonadIO m) => Block -> FilterT m Block
executeBlock cb@(CodeBlock (x, y, alist) input) =
  case lookup "exec" alist of
    Just cmd -> CodeBlock (x, y, alist) <$> execute input cmd
    Nothing  -> return cb
executeBlock x = return x

--------------------------------------------------------------------------------
execute :: (MonadIO m) => String -> String -> FilterT m String
execute input cmd = do
  (exitcode, sout, _) <- liftIO (readProcessWithExitCode "/bin/sh" args input')

  case (exitcode, sout) of
    (ExitSuccess, x) -> return x
    _ -> throwError (Error $ "command failed: " ++ cmd)

  where
    -- Pandoc doesn't include final newline.
    input' :: String
    input' = if null input then input else input ++ "\n"

    args :: [String]
    args = ["-c", cmd]
