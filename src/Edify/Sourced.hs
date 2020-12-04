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
module Edify.Sourced
  ( Sourced (..),
    Location,
  )
where

import Edify.JSON

-- | FIXME: Write documentation for Location
--
-- @since 0.5.0.0
type Location = Maybe FilePath

-- | FIXME: Write documentation for Type
--
-- @since 0.5.0.0
data Sourced a = Sourced
  { sourced :: a,
    location :: Location
  }
  deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
  deriving (ToJSON, FromJSON) via GenericJSON (Sourced a)
