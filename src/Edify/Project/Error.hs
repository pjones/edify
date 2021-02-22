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
module Edify.Project.Error
  ( Error (..),
    renderError,
  )
where

import qualified Edify.Input as Input
import qualified Edify.Text.Pretty as P

-- | Error that may occur while resolving project configuration.
--
-- @since 0.5.0.0
data Error
  = -- | No input files were given.
    MissingInputFilesError
  | -- | No targets were configured.
    MissingTargetsError
  | -- | Errors triggered while reading configuration files.
    ConfigInputError Input.Error
  | -- | Unable to create a valid file extension from a target
    -- configuration.
    InvalidTargetFileExtensionError Text
  | -- | Target command format string can't be parsed.
    TargetCommandInvalidFormatError Text String
  | -- | Target command format string contains invalid variables.
    TargetCommandInvalidVarsError Text [Char] [Char]
  deriving stock (Generic, Show)

-- | Render an 'Error'.
--
-- @since 0.5.0.0
renderError :: Error -> P.Doc P.AnsiStyle
renderError = \case
  MissingInputFilesError ->
    P.reflow "no markdown files were given as input"
  MissingTargetsError ->
    P.reflow "project has no configured targets"
  ConfigInputError e ->
    P.fillSep
      [ P.reflow "project configuration",
        Input.renderError e
      ]
  InvalidTargetFileExtensionError text ->
    P.fillSep
      [ P.reflow "unable to translate target name into a valid file extension:",
        P.dquotes (P.red (P.reflow text)),
        P.reflow "cannot be made into a valid file extension."
      ]
  TargetCommandInvalidFormatError cmd msg ->
    P.fillSep
      [ P.reflow "a target's command isn't a valid format string.",
        P.reflow "While parsing",
        P.command cmd,
        P.reflow "the parser produced an error:",
        P.annotate (P.color P.Red) (P.reflow $ toText msg)
      ]
  TargetCommandInvalidVarsError cmd bound free ->
    let ppVar color = map (color . ("%" <>) . P.pretty)
        ppFree = ppVar P.red free
        ppBound = ppVar P.green bound
        ppList items = P.nest 2 (P.fillSep $ P.punctuate P.comma items)
     in P.vcat
          [ P.reflow "target's command contains invalid variables:",
            P.nest 2 (P.hardline <> P.command cmd) <> P.hardline,
            P.fillSep ["invalid variables:", ppList ppFree],
            P.fillSep ["allowed variables:", ppList ppBound]
          ]
