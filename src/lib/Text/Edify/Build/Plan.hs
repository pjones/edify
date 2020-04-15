{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Generate a build plan for a single Markdown file.
module Text.Edify.Build.Plan
  ( plan
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Development.Shake ((%>), need, want, command_)
import qualified Development.Shake as Shake

--------------------------------------------------------------------------------
-- Project Imports:
import qualified Text.Edify.Build.Markdown as Markdown
import Text.Edify.Build.Options (Options(..))
import Text.Edify.Build.Target (Target(..))
import Text.Edify.Build.Template

--------------------------------------------------------------------------------
-- | Given a 'Target', generate all output files as needed.
plan :: Options -> Target -> Shake.Rules ()
plan opts target@Target{..} = do
  want [ targetOutputFile ]

  targetOutputFile %> \out -> do
    need [ targetIntermediateFile ]
    command_ [] "pandoc"
      (pandocArgsForTarget opts target out
       ++ [targetIntermediateFile])

  targetIntermediateFile %> \out -> do
    (doc, deps) <- Markdown.parse target
    need (targetInputFile:deps)
    err <- Markdown.write doc out
    case err of
      Nothing -> return ()
      Just s  -> fail (toString s)

--------------------------------------------------------------------------------
-- | Calculate all of the flags to send to the @pandoc@ executable.
pandocArgsForTarget :: Options -> Target -> FilePath -> [String]
pandocArgsForTarget Options{..} Target{..} out =
  case (targetOutputFormat, targetTemplateStyle) of
    (PDF, Handout) -> basic ++ pdfHandout
    (PDF, Slides)  -> basic ++ beamer
    _              -> basic

  where
    basic =
      [ "--from=markdown" ++ extensions
      , "--filter=pandoc-citeproc"
      , "--output", out
      ] ++ templateFile ++ variables

    beamer =
      [ "--to=beamer"
      , "--slide-level=3"
      , "--variable=classoption:aspectratio=43"
      ]

    pdfHandout =
      [ "--to=latex"
      , "--toc", "--toc-depth=2"
      , "--top-level-division=chapter"
      , "--number-sections"
      ]

    templateFile =
      case targetTemplateFile of
        Nothing   -> []
        Just file -> ["--template", file]

    variables =
      map (\(k, v) -> "--variable=" ++ k ++ ":" ++ v)
        targetPandocVariables

    extensions =
      if null optionsMarkdownExtensions then ""
      else "+" ++ intercalate "+" optionsMarkdownExtensions
