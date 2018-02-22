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
-- | Build rules for other generated files such as diagrams.
module Text.Edify.Build.Rules
  ( rules
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Development.Shake ((%>), need, cmd, unit)
import qualified Development.Shake as Shake
import System.FilePath

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Edify.Build.Options (Options(..))

--------------------------------------------------------------------------------
-- | Returns a 'Shake.Rules' value with instructions on how to build
-- various dependencies used in Markdown files.
rules :: Options -> Shake.Rules ()
rules Options{..} = do

  ------------------------------------------------------------------------------
  -- How to build Graphviz DOT files:
  "//*.dot.pdf" %> \out -> do
    let src = makeRelative optionsOutputDirectory (dropExtension out)
        ps  = out -<.> ".ps"
        raw = out -<.> ".rawpdf"

    need [ src ]
    unit $ cmd "dot" ["-Tps", "-o", ps, src]
    unit $ cmd "ps2pdf" [ps, raw]
    unit $ cmd "pdfcrop" [raw, out]
