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
module Edify.JSON
  ( GenericJSON (..),
    RecursiveJSON (..),
    Aeson.ToJSON,
    Aeson.FromJSON,

    -- * Parsing w/ Default Values
    Readiness (..),
    Parsed,
    Resolved,
    Default,
    Checked,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Functor.Foldable as Recursion
import GHC.Generics (Rep)

-- | Type wrapper for automatic JSON deriving.
newtype GenericJSON a = GenericJSON
  {genericJSON :: a}

-- | Default JSON decoding/encoding options.
aesonOptions :: Aeson.Options
aesonOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = kebabCase >>> dropFirstWord,
      Aeson.constructorTagModifier = kebabCase,
      Aeson.allNullaryToStringTag = True,
      Aeson.omitNothingFields = True,
      Aeson.sumEncoding = Aeson.ObjectWithSingleField
    }
  where
    dropFirstWord = dropWhile (/= '-') >>> drop 1
    kebabCase = Aeson.camelTo2 '-'

instance
  ( Generic a,
    Aeson.GToJSON Aeson.Zero (Rep a),
    Aeson.GToEncoding Aeson.Zero (Rep a)
  ) =>
  Aeson.ToJSON (GenericJSON a)
  where
  toJSON = Aeson.genericToJSON aesonOptions . genericJSON
  toEncoding = Aeson.genericToEncoding aesonOptions . genericJSON

instance
  ( Generic a,
    Aeson.GFromJSON Aeson.Zero (Rep a)
  ) =>
  Aeson.FromJSON (GenericJSON a)
  where
  parseJSON = fmap GenericJSON . Aeson.genericParseJSON aesonOptions

-- | Aeson instances for types using @recursion-schemes@.
newtype RecursiveJSON r = RecursiveJSON r

instance
  ( Recursion.Recursive r,
    Aeson.ToJSON (Recursion.Base r Aeson.Value)
  ) =>
  Aeson.ToJSON (RecursiveJSON r)
  where
  toJSON (RecursiveJSON r) = Recursion.cata Aeson.toJSON r

instance
  ( Recursion.Corecursive r,
    Aeson.FromJSON (Recursion.Base r r)
  ) =>
  Aeson.FromJSON (RecursiveJSON r)
  where
  parseJSON = fmap (RecursiveJSON . Recursion.embed) . Aeson.parseJSON

-- | States of readiness.
--
-- @since 0.5.0.0
data Readiness
  = -- | A value has been parsed but not validated.
    Parsed
  | -- | A value has been resolved to its final value.
    Resolved

-- | The 'Readiness' data constructor promoted to a type.
--
-- @since 0.5.0.0
type Parsed = 'Parsed

-- | The 'Readiness' data constructor promoted to a type.
--
-- @since 0.5.0.0
type Resolved = 'Resolved

-- | A type family for types that have a default value.
type family Default (f :: Readiness) a where
  Default Parsed a = Maybe a
  Default Resolved a = a

-- | A type family for parsing as one type, but resolve to a safer
-- more restricted type.
type family Checked (f :: Readiness) a b where
  Checked Parsed a _ = a
  Checked Resolved _ b = b
