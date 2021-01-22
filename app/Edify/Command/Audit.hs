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
import qualified Edify.Compiler.Project as Project
import qualified Options.Applicative as Opt

-- | Options that affect audits.
--
-- @since 0.5.0.0
data Flags (f :: Type -> Type) = Flags
  { flagsOutputMode :: Audit.Mode,
    flagsCompilerOptions :: Options.OptionsF f
  }
  deriving stock (Generic)

-- | Command description and option parser.
--
-- @since 0.5.0.0
desc :: (String, Opt.Parser (Flags Maybe))
desc = ("Audit one or more projects", flags)
  where
    flags :: Opt.Parser (Flags Maybe)
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
        <*> Options.fromCommandLine Project.fromMinimalCommandLine

-- | Resolve all options to their final values.
--
-- @since 0.5.0.0
resolve :: MonadIO m => Flags Maybe -> m (Either Options.Error (Flags Identity))
resolve Flags {..} =
  Options.resolve flagsCompilerOptions
    <&> second (Flags flagsOutputMode)

-- | Execute a build.
--
-- @since 0.5.0.0
main :: Flags Maybe -> IO ()
main = resolve >=> go
  where
    go :: Either Options.Error (Flags Identity) -> IO ()
    go = \case
      Left e -> die (show e)
      Right Flags {..} -> Audit.main flagsOutputMode flagsCompilerOptions
