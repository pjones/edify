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
module Edify.Command.Generate
  ( Flags,
    desc,
    main,
  )
where

import Control.Lens ((^.))
import qualified Edify.Compiler.FilePath as FilePath
import qualified Edify.Compiler.User as User
import qualified Edify.Input as Input
import qualified Edify.Project as Project
import qualified Edify.System.Exit as Exit
import qualified Options.Applicative as Opt
import qualified System.Directory as Directory

-- | What do we want to generate?
--
-- @since 0.5.0.0
data What
  = ProjectConfig

-- | Command line options.
--
-- @since 0.5.0.0
newtype Flags = Flags
  {flagsWhat :: What}

-- | Command line option parser for 'What' values.
--
-- @since 0.5.0.0
whatP :: Opt.Parser What
whatP =
  asum
    [ Opt.flag'
        ProjectConfig
        ( mconcat
            [ Opt.long "project",
              Opt.short 'p',
              Opt.help "Generate a project configuration file"
            ]
        )
    ]

-- | Command description.
--
-- @since 0.5.0.0
desc :: (String, Opt.Parser Flags)
desc = ("Generate a basic configuration file", flags)
  where
    flags :: Opt.Parser Flags
    flags = Flags <$> whatP

-- | Main entry point.
--
-- @since 0.5.0.0
main :: User.User -> Flags -> IO ()
main user Flags {..} = case flagsWhat of
  ProjectConfig -> do
    pwd <- Directory.getCurrentDirectory
    file <- FilePath.makeAbsoluteToDir pwd Project.defaultProjectConfigFile

    Input.encodeToFile'
      ( Input.WriteFingerprintTo
          (user ^. #userCommandAllowDir)
          Project.projectCommands
      )
      file
      (const (toLazy Project.defaultProjectConfigBytes))
      Project.defaultProjectConfig
      >>= either (Exit.withError . Input.renderError) pure
