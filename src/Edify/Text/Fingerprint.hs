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
module Edify.Text.Fingerprint
  ( Fingerprint (..),
    Fingerprinted (..),
    Status (..),
    Commands,
    Cache,
    cache,
    read,
    write,
  )
where

import Control.Lens (at, (.~), (^.))
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Base16 as Base16
import Data.Generics.Labels ()
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Edify.System.FilePath ((</>))
import Edify.Text.JSON
import qualified System.Directory as Directory

-- | A fingerprint for a file.
--
-- @since 0.5.0.0
data Fingerprint a = Fingerprint
  { -- | The file that is being fingerprinted.
    fingerprintFile :: FilePath,
    -- | The content of the fingerprint.
    fingerprintContent :: a
  }
  deriving stock (Generic, Eq, Show, Functor, Foldable, Traversable)
  deriving (ToJSON, FromJSON) via GenericJSON (Fingerprint a)

-- | Fingerprint status.
--
-- @since 0.5.0.0
data Status
  = -- | The fingerprint matches.
    Verified
  | -- | The fingerprint does not match.
    Mismatch
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via GenericJSON Status

class Fingerprinted a where
  type Subject a
  generate :: Subject a -> a
  verify :: Subject a -> a -> Status

-- | Individual fingerprints for each shell command mentioned in the
-- corresponding file.
--
-- @since 0.5.0.0
newtype Commands = Commands
  {unCommands :: HashSet Text}
  deriving stock (Generic, Show)
  deriving newtype (Semigroup, Monoid)
  deriving (ToJSON, FromJSON) via GenericJSON Commands

instance Fingerprinted Commands where
  type Subject Commands = [Text]
  generate cmds =
    map (encodeUtf8 >>> SHA256.hash >>> Base16.encode >>> decodeUtf8) cmds
      & fromList
      & Commands
  verify cmds (Commands setA) =
    let Commands setB = generate cmds
     in if all (`HashSet.member` setA) setB
          then Verified
          else Mismatch

-- | Fingerprint cache.
--
-- @since 0.5.0.0
newtype Cache a = Cache
  {unCache :: HashMap ByteString (Fingerprint a)}
  deriving stock (Generic, Show)
  deriving newtype (Semigroup, Monoid)

-- | Build a cache for the given file and entry.
--
-- @since 0.5.0.0
cache ::
  Semigroup a =>
  FilePath ->
  a ->
  Cache a ->
  Cache a
cache file a (Cache cs) =
  let key = cacheKey file
   in Cache $
        HashMap.insertWith
          ( \x y ->
              Fingerprint
                file
                (on (<>) fingerprintContent x y)
          )
          key
          (Fingerprint file a)
          cs

-- | Read the fingerprint for a file from the cache or from the
-- fingerprint directory.
--
-- @since 0.5.0.0
read ::
  forall m a.
  MonadIO m =>
  FromJSON a =>
  -- | Existing cache.
  Cache a ->
  -- | The directory that contains the fingerprint files.
  FilePath ->
  -- | The file that is being fingerprinted.  This path should be an
  -- absolute path.
  FilePath ->
  -- | Result with updated cache.  Nothing indicates that no
  -- fingerprint exists for the given file.
  m (Maybe (Fingerprint a), Cache a)
read (Cache cache) dir file =
  let key = cacheKey file
   in case cache ^. at key of
        Just fp ->
          pure (Just fp, Cache cache)
        Nothing -> do
          fp <- load key
          pure (fp, Cache (cache & at key .~ fp))
  where
    load :: ByteString -> m (Maybe (Fingerprint a))
    load key = runMaybeT $ do
      let file = cachePath dir key
      guardM (liftIO $ Directory.doesFileExist file)
      MaybeT (readFileLBS file <&> Aeson.decode)

-- | Write a fingerprint cache to disk.
--
-- @since 0.5.0.0
write ::
  MonadIO m =>
  ToJSON a =>
  -- | Directory where allow files are stored.
  FilePath ->
  -- | The cache to write.
  Cache a ->
  m ()
write dir (Cache cache) =
  for_ (HashMap.toList cache) $ \(key, entry) -> do
    let file = cachePath dir key
    liftIO (Directory.createDirectoryIfMissing True dir)
    writeFileLBS file (Pretty.encodePretty entry)

-- | Generate a cache key.
cacheKey :: FilePath -> ByteString
cacheKey =
  encodeUtf8
    >>> SHA256.hash
    >>> Base16.encode

-- | Generate a cache file name.
cachePath :: FilePath -> ByteString -> FilePath
cachePath dir key = dir </> decodeUtf8 key
