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
module Edify.Compiler.Project
  ( ProjectF (..),

    -- * Parsing Project Options
    fromCommandLine,

    -- * Resolving a Final Value
    Error (..),
    resolve,
  )
where

import Edify.JSON
import Edify.Text.Indent (Tabstop (..), defaultTabstop)
import qualified Options.Applicative as Opt
import qualified System.Directory as Directory

-- | Project-level configuration.
--
-- @since 0.5.0.0
data ProjectF (f :: Type -> Type) = Project
  { -- | The directory where all generated files will be placed.
    projectOutputDirectory :: Default f FilePath,
    -- | The width of a tab character when converted to spaces.
    projectTabstop :: Default f Tabstop,
    -- | List of input Markdown files to process.
    projectInputFiles :: Default f (NonEmpty FilePath)
  }
  deriving stock (Generic)

instance Semigroup (ProjectF Maybe) where
  (<>) x y =
    Project
      { projectOutputDirectory =
          projectOutputDirectory x <|> projectOutputDirectory y,
        projectInputFiles =
          projectInputFiles x <|> projectInputFiles y,
        projectTabstop =
          projectTabstop x <|> projectTabstop y
      }

instance Monoid (ProjectF Maybe) where
  mempty =
    Project
      { projectOutputDirectory = Nothing,
        projectInputFiles = Nothing,
        projectTabstop = Nothing
      }

deriving via (GenericJSON (ProjectF Maybe)) instance ToJSON (ProjectF Maybe)

deriving via (GenericJSON (ProjectF Maybe)) instance FromJSON (ProjectF Maybe)

-- | Parse project configuration on the command line.
--
-- @since 0.5.0.0
fromCommandLine :: Opt.Parser (ProjectF Maybe)
fromCommandLine =
  Project
    <$> optional
      ( Opt.strOption $
          mconcat
            [ Opt.long "output",
              Opt.short 'o',
              Opt.metavar "DIR",
              Opt.help "Project output directory directory"
            ]
      )
    <*> optional
      ( Opt.option (Tabstop <$> Opt.auto) $
          mconcat
            [ Opt.long "tabstop",
              Opt.short 'T',
              Opt.metavar "NUM",
              Opt.help "Change the width of a tab character"
            ]
      )
    <*> optional
      ( fromList
          <$> some
            ( Opt.strArgument $
                mconcat
                  [ Opt.metavar "FILE",
                    Opt.help "Markdown file to process"
                  ]
            )
      )

-- | Error that may occur while resolving project configuration.
--
-- @since 0.5.0.0
data Error = MissingInputFilesError
  deriving stock (Generic, Show)

-- | Resolve all values in a project configuration.
--
-- @since 0.5.0.0
resolve :: MonadIO m => ProjectF Maybe -> ExceptT Error m (ProjectF Identity)
resolve Project {..} = do
  -- The list of input markdown files is mandatory and made absolute
  -- to the current directory which should already be the project
  -- input directory.
  files <-
    hoistEither (required MissingInputFilesError projectInputFiles)
      >>= traverse (liftIO . Directory.canonicalizePath)

  -- The directory where output files are stored.
  output <-
    fromMaybe ".edify" projectOutputDirectory
      & Directory.canonicalizePath
      & liftIO

  pure
    Project
      { projectOutputDirectory = output,
        projectInputFiles = files,
        projectTabstop = defaultTabstop
      }
  where
    required :: Error -> Maybe a -> Either Error a
    required e = \case
      Nothing -> Left e
      Just x -> Right x
