{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Helper functions for dealing with errors.
module Text.Edify.Util.Error
       ( maybeToEither
       , eitherAlt
       ) where

--------------------------------------------------------------------------------
-- | Convert a @Maybe b@ to an @Either a b@.
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a = maybe (Left a) Right

--------------------------------------------------------------------------------
-- | Fake @Alternative@-like interface for @Either a b@.  If the first
-- @Either@ is a @Right@, return it.  Otherwise return the second
-- @Either@.
eitherAlt :: Either a b -> Either a b -> Either a b
eitherAlt x@(Right _) _ = x
eitherAlt _           y = y
