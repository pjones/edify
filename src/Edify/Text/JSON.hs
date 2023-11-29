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
module Edify.Text.JSON
  ( GenericJSON (..),
    GenericJSON1 (..),
    Aeson.ToJSON,
    Aeson.FromJSON,
    Aeson.ToJSON1,
    Aeson.FromJSON1,

    -- * Two Types, One JSON Document
    (:*:) (..),

    -- * Parsing w/ Default Values
    Readiness (..),
    Parsed,
    Resolved,
    Default,
    Checked,

    -- * Re-exports
    Generic1,
  )
where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as KeyMap
import GHC.Generics (Generic1, Rep, Rep1)
import qualified Generics.SOP as SOP

-- | Default JSON decoding/encoding options.
aesonOptions :: Aeson.Options
aesonOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = kebabCase >>> dropFirstWord,
      Aeson.constructorTagModifier = kebabCase,
      Aeson.allNullaryToStringTag = True,
      Aeson.omitNothingFields = True,
      Aeson.rejectUnknownFields = True,
      Aeson.sumEncoding = Aeson.ObjectWithSingleField
    }
  where
    dropFirstWord = dropWhile (/= '-') >>> drop 1
    kebabCase = Aeson.camelTo2 '-'

-- | Type wrapper for automatic JSON deriving.
newtype GenericJSON a = GenericJSON
  {genericJSON :: a}

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

-- | Type wrapper for automatic JSON1 deriving.
newtype GenericJSON1 f a = GenericJSON1
  {genericJSON1 :: f a}

instance
  ( Generic1 f,
    Aeson.GFromJSON Aeson.One (Rep1 f)
  ) =>
  Aeson.FromJSON1 (GenericJSON1 f)
  where
  liftParseJSON f g v = GenericJSON1 <$> Aeson.genericLiftParseJSON aesonOptions f g v

instance
  ( Generic1 f,
    Aeson.GToJSON' Aeson.Value Aeson.One (Rep1 f),
    Aeson.GToJSON' Aeson.Encoding Aeson.One (Rep1 f)
  ) =>
  Aeson.ToJSON1 (GenericJSON1 f)
  where
  liftToJSON f g = Aeson.genericLiftToJSON aesonOptions f g . genericJSON1
  liftToEncoding f g = Aeson.genericLiftToEncoding aesonOptions f g . genericJSON1

-- | Join two types together so they work with the same JSON document.
newtype (:*:) a b = Join
  {unJoin :: (a, b)}
  deriving stock (Generic, Show)
  deriving newtype (Eq)

instance Lens.Field1 (a :*: b) (a' :*: b) a a' where
  _1 = Lens.lens (fst . unJoin) (\(Join t) -> Join . (,snd t))

instance Lens.Field2 (a :*: b) (a :*: b') b b' where
  _2 = Lens.lens (snd . unJoin) (\(Join t) -> Join . (fst t,))

instance (Aeson.ToJSON a, Aeson.ToJSON b) => Aeson.ToJSON (a :*: b) where
  toJSON prod =
    case bimap Aeson.toJSON Aeson.toJSON (unJoin prod) of
      (Aeson.Object x, Aeson.Object y) -> Aeson.Object (x <> y)
      (x, _) -> x

instance
  ( SOP.IsProductType a xs,
    SOP.HasDatatypeInfo a,
    Aeson.FromJSON a,
    Aeson.FromJSON b
  ) =>
  Aeson.FromJSON (a :*: b)
  where
  parseJSON = \case
    Aeson.Object v ->
      let leftFields =
            SOP.datatypeInfo (Proxy :: Proxy a)
              & SOP.constructorInfo
              & fieldNames
              & map Aeson.Key.fromText
          leftObj = KeyMap.filterWithKey (\k _v -> k `elem` leftFields) v
          rightObj = KeyMap.difference v leftObj
       in fmap
            Join
            ( (,)
                <$> Aeson.parseJSON (Aeson.Object leftObj)
                <*> Aeson.parseJSON (Aeson.Object rightObj)
            )
    _others ->
      fail "expected a JSON object"
    where
      fieldNames :: SOP.NP SOP.ConstructorInfo '[cs] -> [Text]
      fieldNames =
        SOP.hd >>> \case
          SOP.Constructor {} -> []
          SOP.Infix {} -> []
          SOP.Record _ fields ->
            SOP.hliftA (SOP.K . SOP.fieldName) fields
              & SOP.hcollapse
              & map (toText . Aeson.fieldLabelModifier aesonOptions)

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
