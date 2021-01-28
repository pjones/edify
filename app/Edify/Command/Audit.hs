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
module Edify.Command.Audit
  ( Flags,
    desc,
    main,
  )
where

import qualified Edify.Compiler.Audit as Audit
import qualified Edify.Compiler.Options as Options
import Edify.JSON
import qualified Options.Applicative as Opt

-- | Options that affect audits.
--
-- @since 0.5.0.0
data Flags (f :: Readiness) = Flags
  { flagsOutputMode :: Audit.Mode,
    flagsCompilerOptions :: Options.OptionsF f,
    -- FIXME: Should this try to use the input files from the project config?
    flagsInputFiles :: NonEmpty FilePath
  }
  deriving stock (Generic)

-- | Command description and option parser.
--
-- @since 0.5.0.0
desc :: (String, Opt.Parser (Flags Parsed))
desc = ("Analyze and report any file security issues", flags)
  where
    flags :: Opt.Parser (Flags Parsed)
    flags =
      Flags
        <$> asum
          [ Opt.flag'
              Audit.JsonAuditMode
              ( mconcat
                  [ Opt.long "json",
                    Opt.help "Output a complete, machine-readable JSON doc"
                  ]
              ),
            Opt.flag
              Audit.BlockedCommandAuditMode
              Audit.FullAuditMode
              ( mconcat
                  [ Opt.long "full",
                    Opt.help "Produce a complete audit, not just exec info"
                  ]
              )
          ]
        <*> Options.fromCommandLine
        <*> ( fromList
                <$> some
                  ( Opt.strArgument $
                      mconcat
                        [ Opt.metavar "FILE [FILE ...]",
                          Opt.help "Files to audit"
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
        flagsOutputMode = flagsOutputMode,
        flagsInputFiles = flagsInputFiles
      }

-- | Execute a build.
--
-- @since 0.5.0.0
main :: Flags Parsed -> IO ()
main = resolve >=> go
  where
    go :: Flags Resolved -> IO ()
    go Flags {..} =
      Audit.main
        flagsOutputMode
        flagsCompilerOptions
        flagsInputFiles
