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

import Data.Version (showVersion)
import qualified Edify.Command.Allow as Allow
import qualified Edify.Command.Audit as Audit
import qualified Edify.Command.Build as Build
import qualified Edify.Command.Generate as Generate
import qualified Edify.Compiler.User as User
import qualified Edify.System.Exit as Exit
import qualified Edify.System.Input as Input
import Main.Utf8 (withUtf8)
import qualified Options.Applicative as Options
import Paths_edify (version)
import qualified System.Directory as Directory

-- | Type for the command line parser.
data Command
  = CmdBuild Build.Flags
  | CmdAudit Audit.Flags
  | CmdAllow Allow.Flags
  | CmdGenerate Generate.Flags

-- | Command line parser.
commands :: Options.Parser Command
commands =
  Options.hsubparser
    ( mconcat
        [ mkcmd "build" CmdBuild Build.desc,
          mkcmd "audit" CmdAudit Audit.desc,
          mkcmd "allow" CmdAllow Allow.desc,
          mkcmd "generate" CmdGenerate Generate.desc
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

-- | Print the version number and exit.
versionCmd :: Options.Parser (a -> a)
versionCmd =
  Options.infoOption (showVersion version) $
    mconcat
      [ Options.long "version",
        Options.help "Print version number and exit"
      ]

main :: IO ()
main = withUtf8 $ do
  userConfigFile <- User.defaultUserConfigFile
  exists <- Directory.doesFileExist userConfigFile

  user <-
    if exists
      then loadUserConfig userConfigFile
      else User.resolve mempty

  command <- Options.execParser opts

  Exit.catchSync $
    case command of
      CmdBuild flags -> Build.main user flags
      CmdAudit flags -> Audit.main user flags
      CmdAllow flags -> Allow.main user flags
      CmdGenerate flags -> Generate.main user flags
  where
    opts = Options.info (Options.helper <*> versionCmd <*> commands) mempty

    -- Load user configuration or die.
    loadUserConfig :: FilePath -> IO User.User
    loadUserConfig =
      User.readUserConfig >=> \case
        Left e -> Exit.withError (Input.renderError e)
        Right u -> User.resolve u
