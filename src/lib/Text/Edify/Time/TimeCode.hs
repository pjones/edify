{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Types and functions for working with time codes (e.g. slide
-- duration).
module Text.Edify.Time.TimeCode
       ( TimeCode
       , toSeconds
       , fromSeconds
       , asHHMMSS
       , parse
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import           Control.Applicative
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Parsec hiding (parse)
import           Text.Parsec.Text
import           Text.Printf (printf)

--------------------------------------------------------------------------------
-- | Time duration represented as seconds.
newtype TimeCode = TimeCode Int deriving (Num)

--------------------------------------------------------------------------------
-- | The number of seconds represented by the given @TimeCode@.
toSeconds :: TimeCode -> Int
toSeconds (TimeCode x) = x

--------------------------------------------------------------------------------
-- | Convert seconds into a @TimeCode@.
fromSeconds :: Int -> TimeCode
fromSeconds = TimeCode

--------------------------------------------------------------------------------
-- | Convert a @TimeCode@ to @Text@ in the HH:MM:SS format.
asHHMMSS :: TimeCode -> Text
asHHMMSS (TimeCode n) = convert hs <> ":" <> convert ms <> ":" <> convert ss
  where
    convert :: Int -> Text
    convert = T.pack . printf "%02d"

    hs, ms, ss :: Int
    hs = n `div` 3600
    ms = (n - (hs * 3600)) `div` 60
    ss = n - (hs * 3600 + ms * 60)

--------------------------------------------------------------------------------
-- | Parse a @TimeCode@ out of some text.
parse :: Text -> Either String TimeCode
parse text = case runParser parseHHMMSS () "" text of
  Left e   -> Left ("failed to parse time code: " ++ show e)
  Right tc -> Right tc

--------------------------------------------------------------------------------
-- | Parse a @TimeCode@ in the HH:MM:SS format.
parseHHMMSS :: Parser TimeCode
parseHHMMSS = do
  hs <- read <$> (many1 digit <?> "hours")   <* char ':'
  ms <- read <$> (many1 digit <?> "minutes") <* char ':'
  ss <- read <$> (many1 digit <?> "seconds")
  return $ TimeCode (hs * 3600 + ms * 60 + ss)
