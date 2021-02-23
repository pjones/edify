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

import Control.Lens ((^.))
import qualified Edify.Compiler.Audit as Audit
import qualified Edify.Compiler.User as User
import qualified Edify.Project.Inputs as Project
import Edify.Text.JSON
import qualified Options.Applicative as Opt
import qualified System.Directory as Directory

-- | Options that affect audits.
--
-- @since 0.5.0.0
data Flags = Flags
  { flagsOutputMode :: Audit.Mode,
    flagsProjectInputDir :: Project.TopLevelF Parsed,
    flagsInputFiles :: NonEmpty FilePath
  }

-- | Command description and option parser.
--
-- @since 0.5.0.0
desc :: (String, Opt.Parser Flags)
desc = ("Analyze and report any file security issues", flags)
  where
    flags :: Opt.Parser Flags
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
        <*> Project.topLevelFromCommandLine
        <*> ( fromList
                <$> some
                  ( Opt.strArgument $
                      mconcat
                        [ Opt.metavar "FILE [FILE ...]",
                          Opt.help "Files to audit"
                        ]
                  )
            )

-- | Execute a build.
--
-- @since 0.5.0.0
main :: User.User -> Flags -> IO ()
main user Flags {..} = do
  toplevel <- Project.resolveTopLevel flagsProjectInputDir
  traverse Directory.makeAbsolute flagsInputFiles
    >>= Audit.main
      flagsOutputMode
      (user ^. #userCommandAllowDir)
      (toplevel ^. #projectDirectory)
