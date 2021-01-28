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

import Control.Lens ((^.))
import Data.Generics.Labels ()
import qualified Edify.Compiler.Options as Options
import qualified Edify.Compiler.Project as Project
import qualified Edify.Compiler.Shake as Shake
import Edify.JSON
import qualified Options.Applicative as Opt

-- | Options that affect builds.
--
-- @since 0.5.0.0
data Flags (f :: Readiness) = Flags
  { flagsCommandSafety :: Shake.CommandSafety,
    flagsOnlyTargets :: Maybe (NonEmpty Text),
    flagsCompilerOptions :: Options.OptionsF f,
    flagsProjectOptions :: Project.ProjectF f
  }

-- | Command description and option parser.
--
-- @since 0.5.0.0
desc :: (String, Opt.Parser (Flags Parsed))
desc = ("Build one or more projects", flags)
  where
    flags :: Opt.Parser (Flags Parsed)
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
        <*> Options.fromCommandLine
        <*> Project.fromCommandLine

-- | Resolve all options to their final values.
--
-- @since 0.5.0.0
resolve :: MonadIO m => Flags Parsed -> m (Either Project.Error (Flags Resolved))
resolve Flags {..} = runExceptT $ do
  compiler <- Options.resolve flagsCompilerOptions
  project <- Project.resolve flagsProjectOptions

  pure
    Flags
      { flagsCommandSafety = flagsCommandSafety,
        flagsOnlyTargets = flagsOnlyTargets,
        flagsCompilerOptions = compiler,
        flagsProjectOptions = project
      }

-- | Execute a build.
--
-- @since 0.5.0.0
main :: Flags Parsed -> IO ()
main = resolve >=> go
  where
    go :: Either Project.Error (Flags Resolved) -> IO ()
    go = \case
      Left e -> die (show e)
      Right Flags {..} -> do
        case filterTargets flagsOnlyTargets flagsProjectOptions of
          Nothing ->
            die "--target option does not match any project target"
          Just project ->
            Shake.main flagsCompilerOptions project flagsCommandSafety

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
        let targets = toList (project ^. #projectTargets)
            predicate t = (t ^. #targetName) `elem` names
        list <- nonEmpty (filter predicate targets)
        pure (project {Project.projectTargets = list})
