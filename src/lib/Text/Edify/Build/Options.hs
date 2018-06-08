{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Command-line options for the build command.
module Text.Edify.Build.Options
  ( Options(..)
  , OutputFormat(..)
  , options
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Applicative (many, optional)
import Data.Monoid ((<>))

import Options.Applicative
         ( Parser, option, argument, str
         , long, help, metavar, value, showDefault
         )

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Edify.Build.Template
import qualified Text.Edify.Filter.Options as Filter

--------------------------------------------------------------------------------
-- | Command-line options for the @build@ command.
data Options = Options
  { optionsFilter :: Filter.Options
    -- ^ Command-line options for the filters.

  , optionsOutputDirectory :: FilePath
    -- ^ The directory to store generated files in.

  , optionsProjectDirectory :: Maybe FilePath
    -- ^ The top-level directory for the project being built.  If not
    -- set on the command line it will be calculated automatically.

  , optionsTemplates :: [Template]
    -- ^ A list of templates to build.

  , optionsInputFiles :: [FilePath]
    -- ^ A list of input Markdown files to process.
  }

--------------------------------------------------------------------------------
-- | Command-line parser.
options :: Parser Options
options = Options <$> Filter.options
                  <*> option str outputDirOpt
                  <*> optional (option str projectDirOpt)
                  <*> pure [ BuiltinTemplate PDF Handout
                           , BuiltinTemplate PDF Slides
                           , BuiltinTemplate PDF Outline
                           ]
                  <*> many (argument str (metavar "FILE"))

  where
    outputDirOpt =
      long "output" <> metavar "DIR" <>
      value "build" <> showDefault <>
      help "Directory where output files are created"

    projectDirOpt =
      long "top" <> metavar "DIR" <>
      help "The top-level project directory"
