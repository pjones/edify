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
--
-- Input stack for tracking which file is currently being read.
module Edify.Compiler.Stack
  ( Stack,
    push,
    pop,
    top,
    directory,
    resolve,
  )
where

import System.Directory (getCurrentDirectory)
import System.FilePath (isRelative, normalise, takeDirectory, (</>))

-- | A FIFO stack of elements.
--
-- @since 0.5.0.0
newtype Stack a = Stack
  {unStack :: [a]}
  deriving stock (Generic, Show)
  deriving newtype (Functor, Semigroup, Monoid)

-- | Push an element on to the head of the stack.
--
-- @since 0.5.0.0
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

-- | Pop an element off the head of the stack.
--
-- @since 0.5.0.0
pop :: Stack a -> Stack a
pop = \case
  Stack [] -> Stack []
  Stack (_ : xs) -> Stack xs

-- | Access the top of the stack.
--
-- @since 0.5.0.0
top :: Stack a -> Maybe a
top = unStack >>> listToMaybe

-- | The parent directory for the file at the head of the stack.
--
-- @since 0.5.0.0
directory :: MonadIO m => Stack FilePath -> m FilePath
directory =
  unStack >>> listToMaybe >>> \case
    Nothing -> liftIO getCurrentDirectory
    Just file -> pure (takeDirectory file)

-- | Resolve a file path that is relative to the head of the stack.
--
-- @since 0.5.0.0
resolve :: MonadIO m => FilePath -> Stack FilePath -> m FilePath
resolve file stack = directory stack <&> (`go` file)
  where
    go :: FilePath -> FilePath -> FilePath
    go dir file
      | isRelative file = normalise (dir </> file)
      | otherwise = normalise file
