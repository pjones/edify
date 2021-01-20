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
  ( Asset,
    AssetF (..),
    require,
    input,
    output,
    command,
    rename,
    Ext,
    AssetMap,
    assets,
  )
where

import Control.Monad.Free.Church (F, MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import System.FilePath (dropExtension, takeDirectory, (-<.>))

-- | FIXME: Write documentation for Ext
--
-- @since 0.5.0.0
type Ext = Text

-- | FIXME: Write documentation for AssetMap
--
-- @since 0.5.0.0
type AssetMap = Map Ext (Map Ext Asset)

-- | FIXME: Write documentation for AssetF
--
-- @since 0.5.0.0
data AssetF k
  = -- | Require that another asset be built first.  The name of the
    -- asset is calculated by appending the given extension to the
    -- current input file.
    Require Ext k
  | -- | Get the name of the input file.  This is the file that should
    -- be used to generate the output file.
    Input (FilePath -> k)
  | -- | Get the name of the output file.  This is the file that must
    -- be produced after the asset compiler is done.
    Output (FilePath -> k)
  | -- | Execute a command.
    Command String [String] k
  | -- | Produce the output file by renaming an already generated
    -- file.  The given function is given the name of the output file
    -- and should produce a file that needs to be renamed to produce
    -- the expected output file.
    Rename (FilePath -> FilePath) k
  deriving stock (Functor)

-- | FIXME: Write documentation for Asset
--
-- @since 0.5.0.0
type Asset = F AssetF ()

makeFree ''AssetF

-- | Graphviz @dot -> pdf@.
--
-- @since 0.5.0.0
dot2pdf :: Asset
dot2pdf = do
  src <- input
  dst <- output

  let ps = dst -<.> ".ps"
      raw = dst -<.> ".rawpdf"

  command "dot" ["-Tps", "-o", ps, src]
  command "ps2pdf" [ps, raw]
  command "pdfcrop" [raw, dst]

-- | @svg -> pdf@ by way of Inkscape.
--
-- @since 0.5.0.0
svg2pdf :: Asset
svg2pdf = do
  src <- input
  dst <- output

  command
    "inkscape"
    [ "--without-gui",
      "--export-area-drawing",
      "--export-pdf",
      dst,
      src
    ]

-- | @tex -> pdf@ by way of @latexmk@.
--
-- @since 0.5.0.0
tex2pdf :: Asset
tex2pdf = do
  src <- input
  dst <- output

  command
    "latexmk"
    [ "-norc",
      "-quiet",
      "-pdf",
      "-xelatex",
      "-outdir=" ++ takeDirectory dst,
      src
    ]

  -- The output file name is chosen by latexmk for us ;(
  --
  -- So, do a rename:
  --
  -- foo.pdf -> foo.tex.pdf
  rename (dropExtension >>> (-<.> ".pdf"))

-- | Message Sequence Chart to PDF by way of @mscgen@.
--
-- <http://www.mcternan.me.uk/mscgen/>
--
-- @since 0.5.0.0
msc2pdf :: Asset
msc2pdf = do
  src <- input
  dst <- output

  let eps = dst -<.> ".eps"
      raw = dst -<.> ".rawpdf"

  command "mscgen" ["-T", "eps", "-i", src, "-o", eps]
  command "ps2pdf" [eps, raw]
  command "pdfcrop" [raw, dst]

-- | A map of all asset builders.
--
-- @since 0.5.0.0
assets :: AssetMap
assets =
  fromList
    [ ("dot", fromList [("pdf", dot2pdf)]),
      ("svg", fromList [("pdf", svg2pdf)]),
      ("tex", fromList [("pdf", tex2pdf)]),
      ("msc", fromList [("msc", msc2pdf)])
    ]
