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
    Default,
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

-- | A type family for types that have a default value.
--
-- When the @f@ argument is 'Maybe', the value is wrapped in a 'Maybe'.
--
-- When the @f@ arguments is 'Identity' the type is exposed directly.
type family Default (f :: * -> *) a where
  Default Maybe a = Maybe a
  Default Identity a = a
