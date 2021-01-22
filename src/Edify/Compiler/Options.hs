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
--
-- Options that control the behavior of the Markdown compiler.
module Edify.Compiler.Options
  ( OptionsF (..),
    Options,
    fromCommandLine,

    -- * Resolving Options
    Error (..),
    resolve,
  )
where

import Control.Monad.Except (mapExceptT)
import qualified Edify.Compiler.Project as Project
import qualified Edify.Compiler.User as User
import Edify.JSON
import qualified Options.Applicative as O
import qualified System.Directory as Directory

-- | Options that control how a project will be processed.
--
-- @since 0.5.0.0
data OptionsF (f :: Type -> Type) = Options
  { -- | The top-level project directory.
    optionsProjectDirectory :: Default f FilePath,
    -- | Configuration stored in a user's file system.
    optionsUserConfig :: User.UserF f,
    -- | Configuration specific to the current project.
    optionsProjectConfig :: Project.ProjectF f
  }
  deriving stock (Generic)

instance Semigroup (OptionsF Maybe) where
  (<>) x y =
    Options
      { optionsProjectDirectory =
          optionsProjectDirectory x
            <|> optionsProjectDirectory y,
        optionsUserConfig =
          optionsUserConfig x
            <> optionsUserConfig y,
        optionsProjectConfig =
          optionsProjectConfig x
            <> optionsProjectConfig y
      }

instance Monoid (OptionsF Maybe) where
  mempty =
    Options
      { optionsProjectDirectory = Nothing,
        optionsUserConfig = mempty,
        optionsProjectConfig = mempty
      }

-- | The 'OptionsF' type fully resolved.
--
-- @since 0.5.0.0
type Options = OptionsF Identity

deriving via (GenericJSON (OptionsF Maybe)) instance ToJSON (OptionsF Maybe)

deriving via (GenericJSON (OptionsF Maybe)) instance FromJSON (OptionsF Maybe)

-- | Command line parser.
--
-- @since 0.5.0.0
fromCommandLine ::
  -- | A parser for project-related configuration.
  O.Parser (Project.ProjectF Maybe) ->
  -- | Complete option parser.
  O.Parser (OptionsF Maybe)
fromCommandLine projectParser =
  Options
    <$> optional
      ( O.strOption $
          mconcat
            [ O.long "top",
              O.short 't',
              O.metavar "DIR",
              O.help "Top-level project directory [default: pwd]"
            ]
      )
    <*> pure mempty
    <*> projectParser

-- | Errors that may occur while options are being resolved.
--
-- @since 0.5.0.0
data Error
  = -- | This error only exists for 'mapExceptT.  FIXME: Probably
    -- remove this constructor and dump the Semigroup/Monoid instances.
    RuntimeError !String
  | -- | Errors while resolving the project configuration.
    ProjectConfigError !Project.Error
  deriving stock (Generic, Show)

instance Semigroup Error where
  (<>) = const

instance Monoid Error where
  mempty = RuntimeError "unexpected run time error"

-- | Resolve missing values, setting them to their defaults.
--
-- @since 0.5.0.0
resolve ::
  forall m.
  MonadIO m =>
  OptionsF Maybe ->
  m (Either Error Options)
resolve Options {..} = runExceptT $ do
  top <- locateTopLevelDir
  liftIO (Directory.setCurrentDirectory top)

  user <- User.resolve optionsUserConfig

  project <-
    Project.resolve optionsProjectConfig
      & mapExceptT (fmap (first ProjectConfigError))

  pure
    Options
      { optionsProjectDirectory = top,
        optionsUserConfig = user,
        optionsProjectConfig = project
      }
  where
    locateTopLevelDir :: ExceptT Error m FilePath
    locateTopLevelDir =
      asum -- Pick the first successful of these:
        [ maybe empty (liftIO . Directory.canonicalizePath) optionsProjectDirectory,
          liftIO Directory.getCurrentDirectory
        ]
