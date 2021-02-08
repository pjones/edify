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
module Edify.Compiler.FilePath
  ( makeAbsoluteToDir,
    makeAbsoluteToFile,

    -- * Mapping Output Files to Input Files
    InputExt (..),
    toOutputPath,
    toInputPath,
    toInputPathViaInputExt,

    -- * File Extensions
    Ext (..),
    fromExt,
    takeExtension,
    addExt,
    replaceExt,

    -- * Re-exports
    (</>),
    isDrive,
    takeDirectory,
    takeFileName,
    dropExtension,
  )
where

import qualified Data.ByteString.Base16 as Base16
import Data.Generics.Labels ()
import qualified Data.List as List
import qualified System.Directory as Directory
import System.FilePath (dropExtension, isDrive, takeDirectory, takeFileName, (</>))
import qualified System.FilePath as FilePath

-- | File extension.
--
-- @since 0.5.0.0
newtype Ext = Ext Text
  deriving newtype (Eq, Hashable)

instance Semigroup Ext where
  (<>) (Ext x) (Ext y) = Ext (x <> "." <> y)

-- | Convert 'Ext' to the type of string used by the @filepath@ package.
--
-- @since 0.5.0.0
fromExt :: Ext -> String
fromExt (Ext ext) = "." <> toString ext

-- | Return the extension for the given file.  The extension will not
-- contain the dot prefix.
--
-- @since 0.5.0.0
takeExtension :: FilePath -> Ext
takeExtension =
  FilePath.takeExtension
    >>> drop 1
    >>> toText
    >>> Ext

-- | Append a file extension.
--
-- @since 0.5.0.0
addExt :: FilePath -> Ext -> FilePath
addExt file = (file FilePath.<.>) . fromExt

-- | Replace the file extension on a file.
--
-- @since 0.5.0.0
replaceExt :: FilePath -> Ext -> FilePath
replaceExt file = (file FilePath.-<.>) . fromExt

-- | Make a file absolute using the specified as context.
--
-- @since 0.5.0.0
makeAbsoluteToDir ::
  MonadIO m =>
  -- | The existing absolute path of a directory to compare to.
  FilePath ->
  -- | The relative path to make absolute.
  FilePath ->
  -- | The resolved file path.
  m FilePath
makeAbsoluteToDir dir file
  | FilePath.isAbsolute file =
    liftIO (Directory.canonicalizePath file)
  | otherwise =
    FilePath.normalise dir
      & (</> FilePath.normalise file)
      & Directory.canonicalizePath
      & liftIO

-- | Make a 'FilePath' absolute when it is relative to another
-- 'FilePath'.
--
-- @since 0.5.0.0
makeAbsoluteToFile ::
  MonadIO m =>
  -- | The existing absolute path of a file to compare to.
  FilePath ->
  -- | The relative path to make absolute.
  FilePath ->
  -- | The resolved file path.
  m FilePath
makeAbsoluteToFile context =
  makeAbsoluteToDir (FilePath.takeDirectory context)

-- | Re-parent a file path so it under the output directory.
--
-- @since 0.5.0.0
toOutputPath ::
  -- | Input directory.
  FilePath ->
  -- | Output directory.
  FilePath ->
  -- | File name to translate.
  FilePath ->
  -- | A file in the output directory.
  FilePath
toOutputPath input output file =
  case List.stripPrefix (input <> "/") file of
    Just rel -> output </> rel
    Nothing -> output </> encodePathName file

-- | Re-parent a file path so it is under the input directory.
--
-- @since 0.5.0.0
toInputPath ::
  -- | Input directory.
  FilePath ->
  -- | Output directory.
  FilePath ->
  -- | The File path to translate.
  FilePath ->
  -- | A file in the input directory.
  FilePath
toInputPath input output file =
  case List.stripPrefix (output <> "/") file of
    Nothing -> input </> output -- This should be impossible.
    Just rel -> case decodePathName rel of
      Nothing -> input </> rel
      Just decoded -> decoded

-- | Hints on how to map output file names to input names.
--
-- @since 0.5.0.0
data InputExt
  = -- | The compiler needs a file from the input directory.  The
    -- attached file extension is the extension of the input file.
    FromProjectInput Ext
  | -- | The compiler needs a file that was produced by the output of
    -- another compiler.  Therefore the input file that is needed will
    -- already be in the output directory.
    --
    -- The two file extensions attached to this constructor are:
    --
    -- 1. The original input extension to the previous compiler.
    -- 2. The original output extension to the previous compiler.
    FromBuildProduct Ext Ext

-- | Variant of 'toInputPath' that uses the hints in 'InputExt'.
--
-- @since 0.5.0.0
toInputPathViaInputExt ::
  -- | The input directory.
  FilePath ->
  -- | The output directory.
  FilePath ->
  -- | What we know about the input.
  InputExt ->
  -- | The output file name.
  FilePath ->
  -- | The output file mapped to an input file.
  FilePath
toInputPathViaInputExt indir outdir = \case
  FromProjectInput _ext ->
    -- Example: build/foo.md.slides -> foo.md
    FilePath.dropExtension . toInputPath indir outdir
  FromBuildProduct extIn extOut -> \output ->
    -- Example: build/foo.slides.pdf -> build/foo.md.slides
    let file = FilePath.dropExtension (FilePath.dropExtension output)
     in file <> fromExt extIn <> fromExt extOut

-- | Translate an entire file path into a single file name.  File
-- extensions are preserved.
--
-- @since 0.5.0.0
encodePathName :: FilePath -> FilePath
encodePathName file =
  FilePath.dropExtension file
    & ("edify-" <>)
    & encodeUtf8
    & Base16.encode
    & decodeUtf8
    & (FilePath.<.> FilePath.takeExtension file)

-- | Inverse of 'encodePathName'.
--
-- @since 0.5.0.0
decodePathName :: FilePath -> Maybe FilePath
decodePathName encoded =
  List.stripPrefix "edify-" encoded
    <&> ( \file ->
            FilePath.dropExtension file
              & encodeUtf8
              & Base16.decode
              & fst
              & decodeUtf8
              & (FilePath.<.> FilePath.takeExtension file)
        )
