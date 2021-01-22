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
module Edify.Command.Build
  ( Flags,
    desc,
    main,
  )
where

import qualified Edify.Compiler.Options as Options
import qualified Edify.Compiler.Project as Project
import qualified Edify.Compiler.Shake as Shake
import qualified Options.Applicative as Opt

-- | Options that affect builds.
--
-- @since 0.5.0.0
data Flags (f :: * -> *) = Flags
  { flagsCompilerOptions :: Options.OptionsF f,
    flagsCommandSafety :: Shake.CommandSafety
  }

-- | Command description and option parser.
--
-- @since 0.5.0.0
desc :: (String, Opt.Parser (Flags Maybe))
desc = ("Build one or more projects", flags)
  where
    flags :: Opt.Parser (Flags Maybe)
    flags =
      Flags
        <$> Options.fromCommandLine Project.fromCommandLine
        <*> Opt.flag
          Shake.RequireCommandFingerprints
          Shake.UnsafeAllowAllCommands
          ( mconcat
              [ Opt.long "unsafe-allow-commands",
                Opt.help "Disable safety features and run all commands",
                Opt.hidden
              ]
          )

-- | Resolve all options to their final values.
--
-- @since 0.5.0.0
resolve :: MonadIO m => Flags Maybe -> m (Either Options.Error (Flags Identity))
resolve Flags {..} =
  Options.resolve flagsCompilerOptions
    <&> second (`Flags` flagsCommandSafety)

-- | Execute a build.
--
-- @since 0.5.0.0
main :: Flags Maybe -> IO ()
main = resolve >=> go
  where
    go :: Either Options.Error (Flags Identity) -> IO ()
    go = \case
      Left e -> die (show e)
      Right Flags {..} -> Shake.main flagsCompilerOptions flagsCommandSafety
