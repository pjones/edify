{-# LANGUAGE TypeApplications #-}

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

import Control.Lens ((.~), (^.))
import Data.Generics.Labels ()
import Data.Generics.Product (field')
import qualified Edify.Compiler.Shake as Shake
import qualified Edify.Compiler.User as User
import Edify.JSON
import qualified Edify.Project as Project
import qualified Edify.System.Exit as Exit
import qualified Options.Applicative as Opt

-- | Options that affect builds.
--
-- @since 0.5.0.0
data Flags = Flags
  { flagsCommandSafety :: Shake.CommandSafety,
    flagsOnlyTargets :: Maybe (NonEmpty Text),
    flagsProjectConfig :: Project.ConfigF Parsed,
    flagsProjectTopLevel :: Project.TopLevelF Parsed,
    flagsProjectInputs :: Project.InputsF Parsed
  }

-- | Command description and option parser.
--
-- @since 0.5.0.0
desc :: (String, Opt.Parser Flags)
desc = ("Build one or more projects", flags)
  where
    flags :: Opt.Parser Flags
    flags =
      Flags
        <$> Opt.flag
          Shake.RequireCommandFingerprints
          Shake.UnsafeAllowAllCommands
          ( mconcat
              [ Opt.long "unsafe-allow-commands",
                Opt.help "Disable safety features and run all commands",
                Opt.hidden
              ]
          )
        <*> optional
          ( fromList
              <$> some
                ( Opt.strOption $
                    mconcat
                      [ Opt.long "target",
                        Opt.short 't',
                        Opt.metavar "NAME",
                        Opt.help "Only build targets that match NAME"
                      ]
                )
          )
        <*> Project.configFromCommandLine
        <*> Project.topLevelFromCommandLine
        <*> Project.inputsFromCommandLine

-- | Execute a build.
--
-- @since 0.5.0.0
main :: User.User -> Flags -> IO ()
main user Flags {..} =
  resolve <&> filterTargets flagsOnlyTargets >>= \case
    Nothing ->
      Exit.withError "the --target option does not match any project target"
    Just project ->
      Shake.main user project flagsCommandSafety
  where
    resolve :: IO Project.Project
    resolve =
      Project.resolve
        user
        flagsProjectTopLevel
        flagsProjectInputs
        flagsProjectConfig
        >>= either (Exit.withError . Project.renderError) pure

    filterTargets ::
      -- | User selected list of target names.
      Maybe (NonEmpty Text) ->
      -- | The project to filter.
      Project.Project ->
      -- | The updated project (if possible).
      Maybe Project.Project
    filterTargets = \case
      Nothing -> Just
      Just names -> \project -> do
        let targets = toList (project ^. #projectConfig . #projectTargets)
            predicate t = (t ^. #targetName) `elem` names
        list <- nonEmpty (filter predicate targets)
        pure (project & #projectConfig . field' @"projectTargets" .~ list)
