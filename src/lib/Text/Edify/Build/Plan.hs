{-# LANGUAGE RecordWildCards #-}

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
import Development.Shake ((%>), need, want, cmd)
import qualified Development.Shake as Shake

--------------------------------------------------------------------------------
-- Project Imports:
import qualified Text.Edify.Build.Markdown as Markdown
import Text.Edify.Build.Target (Target(..))
import Text.Edify.Build.Template

--------------------------------------------------------------------------------
-- | Given a 'Target', generate all output files as needed.
plan :: Target -> Shake.Rules ()
plan target@Target{..} = do
  want [ targetOutputFile ]

  targetOutputFile %> \out -> do
    need [ targetIntermediateFile ]
    cmd "pandoc" (pandocArgsForTarget target out ++
                   [targetIntermediateFile])

  targetIntermediateFile %> \out -> do
    (doc, deps) <- Markdown.parse target
    need (targetInputFile:deps)
    err <- Markdown.write doc out
    case err of
      Nothing -> return ()
      Just s  -> fail s

--------------------------------------------------------------------------------
-- | Calculate all of the flags to send to the @pandoc@ executable.
pandocArgsForTarget :: Target -> FilePath -> [String]
pandocArgsForTarget Target{..} out =
  case (targetOutputFormat, targetTemplateStyle) of
    (PDF, Handout) -> basic ++ pdfHandout
    (PDF, Slides)  -> basic ++ beamer
    _              -> basic

  where
    basic =
      [ "--from=markdown"
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
