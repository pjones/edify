{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Types and utility functions for working with templates.
module Text.Edify.Build.Template
  ( OutputFormat(..)
  , TemplateStyle(..)
  , Template(..)
  , parseOutputFormat
  , outputFormatExt
  , filterOptionsForTemplate
  , generateIntermediateFileName
  , resolveTemplate
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as Text
import Options.Applicative (ReadM, eitherReader)
import System.FilePath ((</>), (-<.>))

--------------------------------------------------------------------------------
-- Project Imports:
import Paths_edify (getDataDir)
import Text.Edify.Filter.Options (divClassesToRemove, divClassesToPromote)
import qualified Text.Edify.Filter.Options as Filter

--------------------------------------------------------------------------------
-- | Possible format for the output file.
data OutputFormat = Markdown | PDF | HTML

--------------------------------------------------------------------------------
-- | What style of document to generate.
data TemplateStyle = Handout | Slides | Outline

--------------------------------------------------------------------------------
-- | Enough information to know what kind of output file to generate.
-- FIXME: Poorly named.
data Template = BuiltinTemplate OutputFormat TemplateStyle

--------------------------------------------------------------------------------
-- | Helper function to parse output format names from the command line.
parseOutputFormat :: ReadM OutputFormat
parseOutputFormat = eitherReader (Atto.parseOnly parser . Text.pack)
  where
    parser :: Atto.Parser OutputFormat
    parser =
      (Atto.string "markdown" *> pure Markdown) <|>
      (Atto.string "pdf"      *> pure PDF     ) <|>
      (Atto.string "html"     *> pure HTML    )

--------------------------------------------------------------------------------
-- | Map formats to their file extension.
outputFormatExt :: OutputFormat -> String
outputFormatExt Markdown = ".md"
outputFormatExt PDF      = ".pdf"
outputFormatExt HTML     = ".html"

--------------------------------------------------------------------------------
-- | Customize the filter options based on the style of template being used.
filterOptionsForTemplate :: TemplateStyle
                         -> Filter.Options
                         -> Filter.Options
filterOptionsForTemplate style =
  case style of
    Handout -> removeDivs ["slides-only"]
    Slides  -> removeDivs ["notes"] . promoteDivs ["slides-only"]
    Outline -> removeDivs ["slides-only"] . removeDivs ["notes"]

  where
    removeDivs xs o =
      o {divClassesToRemove = divClassesToRemove o ++ xs}

    promoteDivs xs o =
      o {divClassesToPromote = divClassesToPromote o ++ xs}

--------------------------------------------------------------------------------
-- | Generate the name of the Markdown file that is created from the
-- original input file.
generateIntermediateFileName :: TemplateStyle  -- ^ Style of output.
                             -> FilePath       -- ^ The input file name.
                             -> FilePath       -- ^ Generated output file name.
generateIntermediateFileName style file =
   (file -<.>) $ case style of
                   Handout -> ".handout"
                   Slides  -> ".slides"
                   Outline -> ".outline"

--------------------------------------------------------------------------------
-- | Expand the information in a 'Template'.
resolveTemplate :: (MonadIO m)
                => Template
                -> m (Maybe FilePath, OutputFormat, TemplateStyle)

resolveTemplate (BuiltinTemplate PDF style) = do
  dir <- (</> "data/templates") <$> liftIO getDataDir
  return $ case style of
             Handout -> (Just (dir </> "handout.tex"), PDF, style)
             Slides  -> (Just (dir </> "slides.tex" ), PDF, style)
             Outline -> (Just (dir </> "handout.tex"), PDF, style)

resolveTemplate (BuiltinTemplate format style) =
  return (Nothing, format, style)
