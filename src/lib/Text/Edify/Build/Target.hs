{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Information about an output file generated from Markdown.
module Text.Edify.Build.Target
  ( Target(..)
  , targetMarkdownExtensions
  , filterEnvFromTarget
  , targetsFromOptions
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import System.FilePath ((</>), (<.>))
import qualified Text.Pandoc.Extensions as Pandoc

--------------------------------------------------------------------------------
-- Project Imports:
import Paths_edify (getDataDir)
import Text.Edify.Build.FilePath (findBaseDirectory)
import Text.Edify.Build.Options
import Text.Edify.Build.Template
import qualified Text.Edify.Filter as Filter
import Text.Edify.Filter.FilterT (Env(..))
import Text.Edify.Util.Markdown

--------------------------------------------------------------------------------
-- | All the details needed to build a single output file.
data Target = Target
  { targetInputFile :: FilePath
    -- ^ The path to the source Markdown file.

  , targetIntermediateFile :: FilePath
    -- ^ The path to the intermediate Markdown file that is generated
    -- while processing the original source file (i.e. after all
    -- filters have run).

  , targetOutputFile :: FilePath
    -- ^ The path to the final output file.

  , targetOutputDirectory :: FilePath
    -- ^ The directory where all generated files should be stored.

  , targetOutputFormat :: OutputFormat
    -- ^ The format for the final output file.

  , targetProjectDirectory :: FilePath
    -- ^ The top-level project directory.

  , targetFilterOptions :: Filter.Options
    -- ^ A list of filter options to use when filtering the input
    -- file.

  , targetTemplateStyle :: TemplateStyle
    -- ^ The type of file to generate as the final output file.

  , targetTemplateFile :: Maybe FilePath
    -- ^ Optional @pandoc@ template to use while generating the final
    -- output file.

  , targetPandocVariables :: [(String, String)]
    -- ^ Additional @pandoc@ variables to set when generating the
    -- final output file.
  }

--------------------------------------------------------------------------------
-- | Return the extensions in use for the given target.
targetMarkdownExtensions :: Target -> Pandoc.Extensions
targetMarkdownExtensions Target{..} =
  toPandocExtensions (Filter.markdownExtensions targetFilterOptions)

--------------------------------------------------------------------------------
-- | Given a 'Target' object, generate the 'Env' object needed to run
-- the filter process.
filterEnvFromTarget :: (MonadIO m) => Target -> Env m
filterEnvFromTarget target@Target{..} =
  Env { envFilters = Filter.filters targetFilterOptions
      , envOptions = targetFilterOptions
      , envFormat  = targetOutputFormat
      , envOutputDirectory = Just targetOutputDirectory
      , envProjectDirectory = Just targetProjectDirectory
      , envPandocExts = targetMarkdownExtensions target
      }

--------------------------------------------------------------------------------
-- | Generate a list of 'Target' values from the command-line options.
targetsFromOptions :: (MonadIO m, MonadFail m) => Options -> m [Target]
targetsFromOptions Options{..} = do
    projectDir <- maybe findBaseDirectory return optionsProjectDirectory
    variables  <- pandocVariables

    fmap concat $ forM optionsInputFiles $ \file ->
      forM optionsTemplates $ \template -> do
        (tfile, format, style) <- resolveTemplate template

        let intermediateFile = optionsOutputDirectory </> generateIntermediateFileName style file
            finalFile = intermediateFile <.> outputFormatExt format
            fopts = filterOptionsForTemplate style optionsFilter

        return Target { targetInputFile = file
                      , targetIntermediateFile = intermediateFile
                      , targetOutputFile = finalFile
                      , targetOutputDirectory = optionsOutputDirectory
                      , targetOutputFormat = format
                      , targetProjectDirectory = projectDir
                      , targetFilterOptions = fopts
                      , targetTemplateStyle = style
                      , targetTemplateFile = tfile
                      , targetPandocVariables = variables
                      }

--------------------------------------------------------------------------------
-- | Generate a list of builtin @pandoc@ variables.
pandocVariables :: (MonadIO m) => m [(String, String)]
pandocVariables = do
  imgdir <- (</> "data/images") <$> liftIO getDataDir

  return [ ("logoemailpdf",   imgdir </> "email.pdf")
         , ("logotwitterpdf", imgdir </> "twitter.pdf")
         , ("logodevalotpdf", imgdir </> "devalot.pdf")
         , ("graphics",       "true")
         ]
