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
module Main
  ( main,
  )
where

import qualified Edify.Command.Audit as Audit
import qualified Edify.Command.Build as Build
import qualified Options.Applicative as Options

-- import Data.Version (showVersion)
-- import Paths_edify (version)

-- | Type for the command line parser.
data Command
  = CmdBuild (Build.Flags Maybe)
  | CmdAudit (Audit.Flags Maybe)

-- | Command line parser.
commands :: Options.Parser Command
commands =
  Options.hsubparser
    ( mconcat
        [ mkcmd "build" CmdBuild Build.desc,
          mkcmd "audit" CmdAudit Audit.desc
        ]
    )
  where
    mkcmd name ctor (desc, flags) =
      Options.command
        name
        ( Options.info
            (ctor <$> flags)
            (Options.progDesc desc)
        )

-- -- | Print the version number and exit.
-- versionCmd :: Parser (a -> a)
-- versionCmd = infoOption versionStr versionMod
--   where
--     versionStr = showVersion version -- From cabal file.
--     versionMod =
--       long "version" <> hidden
--         <> help "Print version number and exit"

main :: IO ()
main =
  Options.execParser opts >>= \case
    CmdBuild flags -> Build.main flags
    CmdAudit flags -> Audit.main flags
  where
    opts = Options.info (Options.helper <*> commands) mempty
