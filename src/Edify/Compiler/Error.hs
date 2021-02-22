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
module Edify.Compiler.Error
  ( Error (..),
    render,
    render',
  )
where

import Control.Lens ((^.))
import qualified Edify.Format as Format
import qualified Edify.Input as Input
import qualified Edify.Markdown.Fence as Fence
import qualified Edify.Project as Project
import qualified Edify.Text.Pretty as P

-- | Errors that might occur during a rewrite.
--
-- @since 0.5.0.0
data Error
  = -- | Errors that are from an internal bug.
    InternalBugError !String
  | -- | Errors from the "Input" module.
    InputError !Input.Input !Input.Error
  | -- | A cycle was found while recording a dependency.
    DependencyCycleError !FilePath !FilePath ![FilePath]
  | -- | Errors from the "Format" module.
    FormatError !Input.Input !Format.Error
  | -- | Failure to parse a markdown file.
    ParseError !Input.Input ![String] !String
  | -- | Failed to parse the output of a div rewrite.
    DivRewriteError !Input.Input !Fence.RewriteError
  | -- | Attempt to run an unverified command.
    CommandBlockedError !FilePath !Text
  deriving stock (Generic, Show)

deriving instance Exception Error

-- | FIXME: Write description for render
--
-- @since 0.5.0.0
render :: Project.Project -> Error -> P.Doc P.AnsiStyle
render project =
  let cwd = project ^. #projectTopLevel . #projectInitialDirectory
      top = project ^. #projectTopLevel . #projectDirectory
      inputs = project ^. #projectInputs . #projectInputFiles
   in render' cwd top inputs

-- | Render an error in a user-friendly way.
--
-- @since 0.5.0.0
render' ::
  -- | Initial current working directory.
  FilePath ->
  -- | Project input directory.
  FilePath ->
  -- | Project files.
  NonEmpty FilePath ->
  -- | The error to render.
  Error ->
  -- | Rendered output.
  P.Doc P.AnsiStyle
render' cwd top inputs = \case
  InternalBugError s ->
    P.fillSep
      [ P.reflow "internal inconsistency:",
        P.red (P.pretty s)
      ]
  InputError input e ->
    P.fillSep
      [ Input.renderError e,
        P.reflow "The error was encountered while processing",
        Input.renderInput top input
      ]
  DependencyCycleError top file files ->
    P.vcat
      [ P.fillSep
          [ P.reflow "import cycle detected.",
            "File",
            ppFile top,
            P.reflow "wants to import",
            ppFile file,
            P.reflow "which would result in an infinite loop.",
            P.hardline
          ],
        P.reflow "The loop looks something like this:",
        P.callout (P.vcat $ map ppFile files)
      ]
  FormatError input e ->
    P.vcat
      [ P.fillSep
          [ P.reflow "while processing",
            Input.renderInput top input
          ],
        P.callout (Format.renderError e)
      ]
  ParseError input context e ->
    P.vcat
      [ P.fillSep
          [ P.reflow "parsing markdown from",
            Input.renderInput top input
          ],
        P.callout (P.red $ P.reflow (toText e)),
        P.reflow "Context:",
        P.callout (P.vcat (map P.pretty context))
      ]
  DivRewriteError input e ->
    let body = case Fence.rewriteBody (Fence.errorRewriteRequest e) of
          Nothing -> mempty
          Just b ->
            P.vcat
              [ P.line,
                P.reflow "Body with parser errors:",
                P.line,
                P.pretty b
              ]
     in mconcat
          [ P.fillSep
              [ P.reflow "while rewriting markdown fenced block from",
                Input.renderInput top input <> ":",
                P.reflow (toText $ Fence.errorMessage e)
              ],
            body
          ]
  CommandBlockedError file cmd ->
    P.vcat
      [ P.fillSep
          [ "file",
            ppFile file,
            P.reflow "contains an unapproved command:"
          ],
        P.callout (P.red $ P.command cmd),
        P.reflow "Please audit the project:",
        P.callout (P.edify "audit" inputFiles)
      ]
  where
    ppFile :: FilePath -> P.Doc ann
    ppFile = P.file (Just top)

    inputFiles :: [P.Doc ann]
    inputFiles = map (P.file $ Just cwd) $ toList inputs
