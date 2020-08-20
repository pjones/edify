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
--
-- Compiling source documents to their final form.
module Edify.Project.Compiler
  ( Compiler,
    compilerFromCommand,
    ExtPair,
    toExtPair,
    unExtPair,
    compilersByExtension,
    toShakeRules,
  )
where

import qualified Development.Shake as Shake
import System.Directory (renameFile)
import System.FilePath
  ( (-<.>),
    dropExtension,
    makeRelative,
    takeDirectory,
    takeExtension,
  )

-- | A @Shake@ action that transforms a source file into a destination
-- file, additionally reporting any dependency files that should
-- trigger a recompilation.
--
-- @since 0.5.0.0
type Compiler =
  -- | The name of the input file.
  FilePath ->
  -- | The name of the output file.
  FilePath ->
  -- | An action that can generate the output file.
  Shake.Action ()

-- | Graphviz @dot -> pdf@.
--
-- @since 0.5.0.0
dot2pdf :: Compiler
dot2pdf src out = do
  let ps = out -<.> ".ps"
      raw = out -<.> ".rawpdf"

  Shake.need [src]
  Shake.command_ [] "dot" ["-Tps", "-o", ps, src]
  Shake.command_ [Shake.EchoStdout False] "ps2pdf" [ps, raw]
  Shake.command_ [Shake.EchoStdout False] "pdfcrop" [raw, out]

-- | @svg -> pdf@ by way of Inkscape.
--
-- @since 0.5.0.0
svg2pdf :: Compiler
svg2pdf src out = do
  Shake.need [src]
  Shake.command_
    [Shake.EchoStdout False]
    "inkscape"
    [ "--without-gui",
      "--export-area-drawing",
      "--export-pdf",
      out,
      src
    ]

-- | @tex -> pdf@ by way of @latexmk@.
--
-- @since 0.5.0.0
tex2pdf :: Compiler
tex2pdf src out = do
  -- The output file name that latexmk will pick for us ;(
  -- (foo.tex.pdf -> foo.pdf)
  let out' = dropExtension out -<.> ".pdf"
  Shake.need [src]
  Shake.command_
    []
    "latexmk"
    [ "-norc",
      "-quiet",
      "-pdf",
      "-xelatex",
      "-outdir=" ++ takeDirectory out,
      src
    ]
  -- foo.pdf -> foo.tex.pdf
  liftIO (renameFile out' out)

-- | Message Sequence Chart to PDF by way of @mscgen@.
--
-- <http://www.mcternan.me.uk/mscgen/>
--
-- @since 0.5.0.0
msc2pdf :: Compiler
msc2pdf src out = do
  let eps = out -<.> ".eps"
      raw = out -<.> ".rawpdf"

  Shake.need [src]
  Shake.command_ [] "mscgen" ["-T", "eps", "-i", src, "-o", eps]
  Shake.command_ [Shake.EchoStdout False] "ps2pdf" [eps, raw]
  Shake.command_ [Shake.EchoStdout False] "pdfcrop" [raw, out]

-- | Create a compiler from the given command.
--
-- The command should be an executable that can be found with the
-- @PATH@ environment variable or an absolute path to an executable.
--
-- When run, the executable will be given two arguments, the name of
-- the source file, and the output file that must be generated.
--
-- @since 0.5.0.0
compilerFromCommand :: Text -> Compiler
compilerFromCommand cmd src out = do
  Shake.need [src]
  Shake.command_ [] (toString cmd) [src, out]

-- | A pair of file extensions to map a source file to a destination file.
--
-- @since 0.5.0.0
newtype ExtPair = ExtPair
  {unExtPair :: ByteString}
  deriving stock (Show)
  deriving newtype (Hashable, Eq, Ord)

-- | Create an 'ExtPair' value from a pair of file extensions.
--
-- The arguments can be file names with extensions or file extensions
-- themselves (with or without the separator dot).
--
-- @since 0.5.0.0
toExtPair :: FilePath -> FilePath -> Maybe ExtPair
toExtPair src dst
  | null src || null dst =
    Nothing
  | otherwise =
    Just
      $ ExtPair
      $ encodeUtf8
        ( forceExtension src
            <> forceExtension dst
        )
  where
    forceExtension :: FilePath -> FilePath
    forceExtension file =
      let ext = takeExtension file
       in if null ext
            then "." <> file
            else ext

-- | The default set of compilers.
--
-- @since 0.5.0.0
compilersByExtension :: [(ExtPair, Compiler)]
compilersByExtension =
  [ (ext ".dot.pdf", dot2pdf),
    (ext ".svg.pdf", svg2pdf),
    (ext ".tex.pdf", tex2pdf),
    (ext ".msc.pdf", msc2pdf)
  ]
  where
    ext :: Text -> ExtPair
    ext = encodeUtf8 >>> ExtPair

-- | Generate a 'Shake.Rules' value.
--
-- @since 0.5.0.0
toShakeRules ::
  -- | The output directory where build files can be stored.
  --
  -- **Assumption:** The source files for the generated rule can be found
  -- by removing the right-most file extension and then making the
  -- resulting file name relative to the output directory.  This would
  -- only work if the output directory and source directory are
  -- siblings.
  FilePath ->
  -- | The extension and compiler to use for the rule.
  (ExtPair, Compiler) ->
  -- | The generated rule.
  Shake.Rules ()
toShakeRules outdir (ExtPair ext, action) =
  ("//*" <> decodeUtf8 ext) Shake.%> \out ->
    let src = dropExtension out & makeRelative outdir
     in action src out
