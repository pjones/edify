{-# LANGUAGE TemplateHaskell #-}

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
module Edify.Compiler.Asset
  ( -- * The asset compiler DSL.
    Asset,
    AssetF (..),
    sizeHints,
    command,
    shell,
    rename,

    -- * Default collection of asset compilers.
    AssetMap,
    assets,
    assetExtension,
  )
where

import Control.Monad.Free.Church (F, MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import qualified Edify.Compiler.FilePath as FilePath
import qualified Edify.Project as Project
import System.FilePath (takeDirectory, takeFileName, (-<.>))

-- | Domain Specific Language (DSL) for compiling assets.
--
-- @since 0.5.0.0
data AssetF k
  = -- | Desired @(Width, Height)@ for raster images.
    SizeHints ((Word, Word) -> k)
  | -- | Execute a command.
    Command String [String] k
  | -- | Execute an shell command.
    Shell String k
  | -- | Rename a file.
    --
    -- Any directory components contained in the given files are
    -- stripped off.  The final file name is /always/ in the output
    -- directory.
    Rename FilePath FilePath k
  deriving stock (Functor)

-- | Free monad for 'AssetF'.
--
-- @since 0.5.0.0
type Asset = F AssetF ()

makeFree ''AssetF

-- | General purpose PDF to PNG converter.
--
-- @since 0.5.0.0
pdf2png :: (FilePath, FilePath) -> Asset
pdf2png (input, output) = do
  (width, height) <- sizeHints
  command
    "convert"
    [ "-geometry",
      show width <> "x" <> show height,
      input,
      output
    ]

-- | Graphviz @dot -> pdf@.
--
-- @since 0.5.0.0
dot2pdf :: (FilePath, FilePath) -> Asset
dot2pdf (input, output) = do
  let ps = output -<.> ".ps"
      raw = output -<.> ".rawpdf"

  command "dot" ["-Tps", "-o", ps, input]
  command "ps2pdf" [ps, raw]
  command "pdfcrop" [raw, output]

-- | Graphviz @dot -> png@.
--
-- @since 0.5.0.0
dot2png :: (FilePath, FilePath) -> Asset
dot2png (input, output) = do
  (width, height) <- sizeHints

  -- Conversion to PNG is a bit weird because you need to set the size
  -- in inches.  We use a DPI value of 100 to compensate.
  let size =
        mconcat
          [ show (width `div` 100) <> ",",
            show (height `div` 100) <> "!"
          ]

  command
    "dot"
    [ "-Tpng",
      "-Gsize=" <> size,
      "-Gdpi=100",
      "-o",
      output,
      input
    ]

-- | @svg -> pdf@ by way of Inkscape.
--
-- @since 0.5.0.0
svg2pdf :: (FilePath, FilePath) -> Asset
svg2pdf (input, output) = do
  command
    "inkscape"
    [ "--export-area-drawing",
      "--export-type=pdf",
      "--export-filename=" <> output,
      input
    ]

-- | @svg -> png@ by way of Inkscape.
--
-- @since 0.5.0.0
svg2png :: (FilePath, FilePath) -> Asset
svg2png (input, output) = do
  (width, _height) <- sizeHints
  command
    "inkscape"
    [ "--export-area-drawing",
      "--export-type=png",
      "--export-width=" <> show width,
      -- "--export-height=" <> show height,
      "--export-filename=" <> output,
      input
    ]

-- | @tex -> pdf@ by way of @latexmk@.
--
-- @since 0.5.0.0
tex2pdf :: (FilePath, FilePath) -> Asset
tex2pdf (input, output) = do
  command
    "latexmk"
    [ "-norc",
      "-quiet",
      "-pdf",
      "-xelatex",
      "-outdir=" ++ takeDirectory output,
      input
    ]

  -- The output file name is chosen by latexmk for us ;(
  --
  -- So, do a rename:
  --
  -- foo.pdf -> foo.tex.pdf
  rename (takeFileName input -<.> ".pdf") output

-- | @tex -> png@ by way of @latexmk@.
--
-- @since 0.5.0.0
tex2png :: (FilePath, FilePath) -> Asset
tex2png (input, output) = do
  let pdf = output -<.> ".pdf"
  tex2pdf (input, pdf)
  pdf2png (pdf, output)

-- | Message Sequence Chart to PDF by way of @mscgen@.
--
-- <http://www.mcternan.me.uk/mscgen/>
--
-- @since 0.5.0.0
msc2pdf :: (FilePath, FilePath) -> Asset
msc2pdf (input, output) = do
  let eps = output -<.> ".eps"
      raw = output -<.> ".rawpdf"

  command "mscgen" ["-T", "eps", "-i", input, "-o", eps]
  command "ps2pdf" [eps, raw]
  command "pdfcrop" [raw, output]

-- | Message Sequence Chart to PNG by way of @mscgen@.
--
-- @since 0.5.0.0
msc2png :: (FilePath, FilePath) -> Asset
msc2png (input, output) = do
  let svg = output -<.> ".svg"
  command "mscgen" ["-T", "svg", "-i", input, "-o", svg]
  svg2pdf (svg, output)

-- | A collection of asset compilers, organized by file extension.
--
-- @since 0.5.0.0
type AssetMap =
  HashMap
    FilePath.Ext
    (Project.Format -> (FilePath, FilePath) -> Asset)

-- | A map of all asset builders.
--
-- @since 0.5.0.0
assets :: AssetMap
assets =
  fromList
    [ ( FilePath.Ext "dot",
        \case
          Project.PDF -> dot2pdf
          Project.HTML -> dot2png
      ),
      ( FilePath.Ext "svg",
        \case
          Project.PDF -> svg2pdf
          Project.HTML -> svg2png
      ),
      ( FilePath.Ext "tex",
        \case
          Project.PDF -> tex2pdf
          Project.HTML -> tex2png
      ),
      ( FilePath.Ext "msc",
        \case
          Project.PDF -> msc2pdf
          Project.HTML -> msc2png
      )
    ]

-- | Get the file extension for an asset given the document format.
--
-- @since 0.5.0.0
assetExtension :: Project.Format -> FilePath.Ext
assetExtension = \case
  Project.PDF -> FilePath.Ext "pdf"
  Project.HTML -> FilePath.Ext "png"
