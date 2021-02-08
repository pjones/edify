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

import qualified Data.Text.Prettyprint.Doc as PP
import qualified Edify.Input as Input
import qualified Prettyprinter.Render.Terminal as PP

-- | Error that may occur while resolving project configuration.
--
-- @since 0.5.0.0
data Error
  = -- | No input files were given.
    MissingInputFilesError
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

-- | FIXME: Write description for renderError
--
-- @since 0.5.0.0
renderError :: Error -> PP.Doc PP.AnsiStyle
renderError = \case
  MissingInputFilesError ->
    "no markdown files were given as input"
  ConfigInputError e ->
    PP.fillSep
      [ "while reading project configuration",
        Input.renderError e
      ]
  InvalidTargetFileExtensionError text ->
    PP.vcat
      [ "unable to translate target name into a valid file extension:",
        PP.fillSep
          [ PP.dquotes (PP.annotate (PP.color PP.Red) (PP.pretty text)),
            "cannot be made into a valid file extension."
          ]
      ]
  TargetCommandInvalidFormatError cmd msg ->
    PP.fillSep
      [ "a target's command isn't a valid format string.",
        "While parsing",
        PP.dquotes (PP.pretty cmd),
        "the parser produced an error:",
        PP.annotate (PP.color PP.Red) (PP.pretty msg)
      ]
  TargetCommandInvalidVarsError cmd bound free ->
    let ppVar color = map (PP.annotate (PP.color color) . ("%" <>) . PP.pretty)
        ppFree = ppVar PP.Red free
        ppBound = ppVar PP.Green bound
        ppList items = PP.nest 2 (PP.fillSep $ PP.punctuate PP.comma items)
        ppCmd = PP.dquotes $ PP.annotate (PP.color PP.Yellow) (PP.pretty cmd)
     in PP.vcat
          [ "target's command contains invalid variables:",
            PP.nest 2 (PP.hardline <> ppCmd) <> PP.hardline,
            PP.fillSep ["invalid variables:", ppList ppFree],
            PP.fillSep ["allowed variables:", ppList ppBound]
          ]
