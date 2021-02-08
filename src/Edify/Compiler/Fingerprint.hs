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
module Edify.Compiler.Fingerprint
  ( Fingerprint,
    fingerprint,
    Fingerprinted (..),
    Status (..),
    Self,
    self,
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
import qualified Data.Set as Set
import Edify.JSON
import qualified System.Directory as Directory
import System.FilePath ((</>))

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

-- | A fingerprint for the entire file.
--
-- @since 0.5.0.0
newtype Self = Self
  {unSelf :: Text}
  deriving stock (Generic, Show)
  deriving newtype (ToJSON, FromJSON, Eq)

instance Semigroup Self where
  (<>) _ y = y

instance Fingerprinted Self where
  type Subject Self = LByteString
  generate bytes =
    SHA256.hashlazy bytes
      & Base16.encode
      & decodeUtf8
      & Self
  verify bs self =
    if generate bs == self
      then Verified
      else Mismatch

-- | Individual fingerprints for each shell command mentioned in the
-- corresponding file.
--
-- @since 0.5.0.0
newtype Commands = Commands
  {unCommands :: Set Text}
  deriving stock (Generic, Show)
  deriving newtype (Semigroup, Monoid)
  deriving (ToJSON, FromJSON) via GenericJSON Commands

instance Fingerprinted Commands where
  type Subject Commands = Text
  generate cmd =
    encodeUtf8 cmd
      & SHA256.hash
      & Base16.encode
      & decodeUtf8
      & one
      & Commands
  verify cmd (Commands setA) =
    let Commands setB = generate cmd
     in if setB `Set.isSubsetOf` setA
          then Verified
          else Mismatch

-- | Generate a fingerprint.
--
-- @since 0.5.0.0
fingerprint :: Fingerprinted a => FilePath -> Subject a -> Fingerprint a
fingerprint file subject = Fingerprint file (generate subject)

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
  Fingerprinted a =>
  FilePath ->
  Subject a ->
  Cache a ->
  Cache a
cache file subject (Cache cs) =
  let key = cacheKey file
      entry = fingerprint file subject
   in Cache $
        HashMap.insertWith
          ( \x y ->
              Fingerprint
                file
                (on (<>) fingerprintContent x y)
          )
          key
          entry
          cs

-- | Fingerprint and cache the contents of the given file.
--
-- @since 0.5.0.0
self :: MonadIO m => FilePath -> m (Cache Self)
self file = do
  bs <- readFileLBS file
  pure (cache file bs mempty)

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
