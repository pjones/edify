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
module Edify.Text.Indent
  ( Tabstop (..),
    defaultTabstop,
    untabify,
    unindent,
    indent,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isSpace)
import Data.Semigroup (Min (..))
import qualified Data.Text as Text

-- | We all know that a tabstop is 8 spaces. But, to extend an
-- olive branch to those that are not enlightened, this type allows the
-- width of a tabstop to be changed.
--
-- @since 0.5.0.0
newtype Tabstop = Tabstop Word8
  deriving stock (Show)
  deriving (ToJSON, FromJSON, Eq, Ord, Num) via Word8

-- | The default width of a tabstop.
--
-- @since 0.5.0.0
defaultTabstop :: Tabstop
defaultTabstop = Tabstop 8

-- | Convert tabs to spaces.
--
-- @since 0.5.0.0
untabify :: Tabstop -> Text -> Text
untabify (Tabstop n) t =
  let spaces = Text.replicate (fromIntegral n) (one ' ')
   in if Text.any (== '\t') t
        then Text.concatMap (\c -> if c == '\t' then spaces else one c) t
        else t

-- | Discover the amount of indentation for the given block of text.
-- The text should have already been processed by 'untabify'.
--
-- @since 0.5.0.0
detectIndent :: [Text] -> Int
detectIndent = \case
  [] -> 0
  ls -> foldMap (Min . leadingSpace) ls & getMin

-- | Remove indentation found in the given text using the amount of
-- leading white-space from the least indented line.
--
-- NOTE: Tabs will be converted into spaces using 'Tabstop' to
-- determine the number of spaces to use.
--
-- @since 0.5.0.0
unindent :: Tabstop -> Text -> Text
unindent = indent 0

-- | Update each line in the given text to have exactly /N/ levels of
-- indentation. Each level of indentation is a single space.
--
-- NOTE: Tabs will be converted into spaces using 'Tabstop' to
-- determine the number of spaces to use.
--
-- @since 0.5.0.0
indent :: Int -> Tabstop -> Text -> Text
indent amount ts = untabify ts >>> Text.lines >>> go >>> Text.unlines
  where
    go :: [Text] -> [Text]
    go lines =
      let current = detectIndent lines
       in if current > amount
            then remove (current - amount) lines
            else add (amount - current) lines

    remove :: Int -> [Text] -> [Text]
    remove = map . Text.drop

    add :: Int -> [Text] -> [Text]
    add n =
      let prefix = Text.replicate n (one ' ')
       in map (prefix <>)

-- | Calculate the amount of indentation for the given line of text.
--
-- @since 0.5.0.0
leadingSpace :: Text -> Int
leadingSpace = Text.takeWhile isHorizontalSpace >>> Text.length
  where
    isHorizontalSpace :: Char -> Bool
    isHorizontalSpace c =
      isSpace c
        && c /= '\r'
        && c /= '\n'
        && c /= '\f'
        && c /= '\v'
