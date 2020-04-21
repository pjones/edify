{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

-- | Command-line options for the build command.
module Text.Edify.Build.Options
  ( Options (..),
    OutputFormat (..),
    options,
  )
where

-- Library Imports:
import Options.Applicative
  ( Parser,
    argument,
    help,
    long,
    metavar,
    option,
    showDefault,
    str,
    value,
  )
-- Project Imports:
import Text.Edify.Build.Template
import qualified Text.Edify.Filter.Options as Filter

-- | Command-line options for the @build@ command.
data Options
  = Options
      { -- | Command-line options for the filters.
        optionsFilter :: Filter.Options,
        -- | The directory to store generated files in.
        optionsOutputDirectory :: FilePath,
        -- | The top-level directory for the project being built.  If not
        -- set on the command line it will be calculated automatically.
        optionsProjectDirectory :: Maybe FilePath,
        -- | A list of templates to build.
        optionsTemplates :: [Template],
        -- | Additional flags to send to Pandoc.
        optionsPandocFlags :: [String],
        -- | A list of input Markdown files to process.
        optionsInputFiles :: [FilePath]
      }

-- | Command-line parser.
options :: Parser Options
options =
  Options <$> Filter.options
    <*> option str outputDirOpt
    <*> optional (option str projectDirOpt)
    <*> pure
      [ BuiltinTemplate PDF Handout,
        BuiltinTemplate PDF Slides,
        BuiltinTemplate PDF Outline
      ]
    <*> many (option str pandocFlags)
    <*> many (argument str (metavar "FILE"))
  where
    outputDirOpt =
      long "output" <> metavar "DIR"
        <> value "build"
        <> showDefault
        <> help "Directory where output files are created"
    projectDirOpt =
      long "top" <> metavar "DIR"
        <> help "The top-level project directory"
    pandocFlags =
      long "pandoc" <> metavar "FLAG"
        <> help "Additional flags to pass to Pandoc, without the dashes"
