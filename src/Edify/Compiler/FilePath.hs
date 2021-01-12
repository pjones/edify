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
  ( makeAbsoluteTo,
    toOutputName,
    toInputName,
  )
where

import Control.Lens ((^.))
import qualified Data.ByteString.Base16 as Base16
import Data.Generics.Labels ()
import qualified Data.List as List
import qualified Edify.Compiler.Options as Options
import System.FilePath ((</>))
import qualified System.FilePath as FilePath

-- | File extension.
--
-- @since 0.5.0.0
type Ext = FilePath

-- | Make a 'FilePath' absolute when it is relative to another
-- 'FilePath'.
--
-- @since 0.5.0.0
makeAbsoluteTo ::
  -- | The existing absolute path of a file to compare to.
  FilePath ->
  -- | The relative path to make absolute.
  FilePath ->
  -- | The resolved file path.
  FilePath
makeAbsoluteTo context file
  | FilePath.isAbsolute file = file
  | otherwise =
    FilePath.normalise context
      & FilePath.takeDirectory
      & (</> FilePath.normalise file)

-- | Translate a file name from a source document to a generated
-- output document.  If the source document is outside the project
-- directory this function will generate a encoded file name that can
-- be decoded with 'toInputName'.
--
-- @since 0.5.0.0
toOutputName :: Options.Options -> FilePath -> Ext -> FilePath
toOutputName options file ext =
  let input = options ^. #optionsProjectDirectory
      output = options ^. #optionsProjectConfig . #projectOutputDirectory
   in (FilePath.<.> ext) $
        case List.stripPrefix (input <> "/") file of
          Just rel -> output </> rel
          Nothing -> output </> encodePathName file

-- | Translate the name of an output document back to the name of the
-- input document.  This is the inverse of 'toOutputName'.
--
-- @since 0.5.0.0
toInputName :: Options.Options -> FilePath -> FilePath
toInputName options file =
  let input = options ^. #optionsProjectDirectory
      output = options ^. #optionsProjectConfig . #projectOutputDirectory
   in FilePath.dropExtension $
        case List.stripPrefix (output <> "/") file of
          Nothing -> input </> output -- This should be impossible.
          Just rel -> case decodePathName rel of
            Nothing -> input </> rel
            Just decoded -> decoded

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

-- | FIXME: Write description for decodePathName
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
