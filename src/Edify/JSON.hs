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
    Aeson.ToJSON,
    Aeson.FromJSON,
  )
where

import qualified Data.Aeson as Aeson
import GHC.Generics (Rep)

-- | Type wrapper for automatic JSON deriving.
newtype GenericJSON a = GenericJSON
  {genericJSON :: a}

-- | Default JSON decoding/encoding options.
aesonOptions :: Aeson.Options
aesonOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = snakeCase >>> dropFirstWord,
      Aeson.constructorTagModifier = snakeCase,
      Aeson.allNullaryToStringTag = True,
      Aeson.omitNothingFields = True,
      Aeson.sumEncoding = Aeson.ObjectWithSingleField
    }
  where
    dropFirstWord = dropWhile (/= '_') >>> drop 1
    snakeCase = Aeson.camelTo2 '_'

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
