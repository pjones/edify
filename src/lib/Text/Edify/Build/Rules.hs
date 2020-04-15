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
import Development.Shake ((%>), need, command_)
import qualified Development.Shake as Shake
import System.Directory (renameFile)
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
  -- Graphviz DOT -> PDF:
  "//*.dot.pdf" %> \out -> do
    let src = srcPath out
        ps  = out -<.> ".ps"
        raw = out -<.> ".rawpdf"

    need [ src ]
    command_ [] "dot" ["-Tps", "-o", ps, src]
    command_ [Shake.EchoStdout False] "ps2pdf"  [ps, raw]
    command_ [Shake.EchoStdout False] "pdfcrop" [raw, out]

  ------------------------------------------------------------------------------
  -- SVG -> PDF:
  "//*.svg.pdf" %> \out -> do
    let src = srcPath out

    need [ src ]
    command_ [] "inkscape"
      [ "--without-gui"
      , "--export-area-drawing"
      , "--export-pdf"
      , out
      , src
      ]

  ------------------------------------------------------------------------------
  -- TEX -> PDF:
  "//*.tex.pdf" %> \out -> do
    let src  = srcPath out
        out' = dropExtension out -<.> ".pdf"

    need [ src ]

    command_ [] "latexmk"
      [ "-norc"
      , "-quiet"
      , "-pdf"
      , "-outdir=" ++ takeDirectory out
      , src
      ]
    liftIO (renameFile out' out)

  ------------------------------------------------------------------------------
  -- MSC -> PDF:
  "//*.msc.pdf" %> \out -> do
    let src = srcPath out
        eps = out -<.> ".eps"
        raw = out -<.> ".rawpdf"

    need [ src ]
    command_ [] "mscgen"  ["-T", "eps", "-i", src, "-o", eps]
    command_ [Shake.EchoStdout False] "ps2pdf"  [eps, raw]
    command_ [Shake.EchoStdout False] "pdfcrop" [raw, out]

  where
    srcPath :: FilePath -> FilePath
    srcPath = makeRelative optionsOutputDirectory . dropExtension
