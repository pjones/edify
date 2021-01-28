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
module Edify.Command.Allow
  ( Flags,
    desc,
    main,
  )
where

import qualified Edify.Compiler.Allow as Allow
import qualified Edify.Compiler.Options as Options
import Edify.JSON
import qualified Options.Applicative as Opt

-- | Command line options.
--
-- @since 0.5.0.0
data Flags (f :: Readiness) = Flags
  { flagsCompilerOptions :: Options.OptionsF f,
    flagsInputFiles :: NonEmpty FilePath
  }

-- | Command description.
--
-- @since 0.5.0.0
desc :: (String, Opt.Parser (Flags Parsed))
desc = ("Authorize the use of commands in the specific files", flags)
  where
    flags :: Opt.Parser (Flags Parsed)
    flags =
      Flags
        <$> Options.fromCommandLine
        <*> ( fromList
                <$> some
                  ( Opt.strArgument $
                      mconcat
                        [ Opt.metavar "FILE [FILE ...]",
                          Opt.help "Files to authorize"
                        ]
                  )
            )

-- | Resolve all options to their final values.
--
-- @since 0.5.0.0
resolve :: MonadIO m => Flags Parsed -> m (Flags Resolved)
resolve Flags {..} = do
  compiler <- Options.resolve flagsCompilerOptions
  pure
    Flags
      { flagsCompilerOptions = compiler,
        flagsInputFiles = flagsInputFiles
      }

-- | Main entry point.
--
-- @since 0.5.0.0
main :: Flags Parsed -> IO ()
main =
  resolve >=> \Flags {..} ->
    Allow.main
      flagsCompilerOptions
      flagsInputFiles
