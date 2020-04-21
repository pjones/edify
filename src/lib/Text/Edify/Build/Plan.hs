{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

-- | Generate a build plan for a single Markdown file.
module Text.Edify.Build.Plan
  ( plan,
  )
where

-- Imports:
import Development.Shake ((%>), command_, need, want)
import qualified Development.Shake as Shake
import qualified Text.Edify.Build.Markdown as Markdown
import Text.Edify.Build.Options (Options (..))
import Text.Edify.Build.Target (Target (..), targetMarkdownExtensions)
import Text.Edify.Build.Template
import qualified Text.Edify.Filter as Filter
import qualified Text.Edify.Util.Markdown as Markdown

-- | Given a 'Target', generate all output files as needed.
plan :: Options -> Target -> Shake.Rules ()
plan opts target@Target {..} = do
  want [targetOutputFile]
  targetOutputFile %> \out -> do
    need [targetIntermediateFile]
    command_
      []
      "pandoc"
      ( pandocArgsForTarget opts target out
          ++ [targetIntermediateFile]
      )
  targetIntermediateFile %> \out -> do
    (doc, deps) <- Markdown.parse target
    need (targetInputFile : deps)
    err <- Markdown.writeMarkdownFile (targetMarkdownExtensions target) doc out
    case err of
      Nothing -> return ()
      Just s -> fail (toString s)

-- | Calculate all of the flags to send to the @pandoc@ executable.
pandocArgsForTarget :: Options -> Target -> FilePath -> [String]
pandocArgsForTarget Options {..} Target {..} out =
  case (targetOutputFormat, targetTemplateStyle) of
    (PDF, Handout) -> basic ++ pdfHandout
    (PDF, Slides) -> basic ++ beamer
    _ -> basic
  where
    basic =
      [ "--from=markdown" ++ extensions,
        "--filter=pandoc-citeproc",
        "--output",
        out
      ]
        ++ templateFile
        ++ variables
        ++ flags
    beamer =
      [ "--to=beamer",
        "--slide-level=3",
        "--variable=classoption:aspectratio=43"
      ]
    pdfHandout =
      [ "--to=latex",
        "--toc",
        "--toc-depth=2",
        "--top-level-division=chapter",
        "--number-sections"
      ]
    templateFile =
      case targetTemplateFile of
        Nothing -> []
        Just file -> ["--template", file]
    variables =
      map
        (\(k, v) -> "--variable=" ++ k ++ ":" ++ v)
        targetPandocVariables
    extensions =
      let exts = Filter.markdownExtensions optionsFilter
       in if null exts then "" else "+" ++ intercalate "+" (map toString exts)
    flags =
      map ("--" <>) optionsPandocFlags
