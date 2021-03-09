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
    stack,
    push,
    pop,
    top,
    bottom,
    directory,
    resolve,
  )
where

import qualified Data.List.NonEmpty as NonEmpty
import System.FilePath (isRelative, normalise, takeDirectory, (</>))

-- | A (non-empty) FIFO stack of elements.
--
-- @since 0.5.0.0
newtype Stack a = Stack
  {unStack :: NonEmpty a}
  deriving stock (Generic, Show)
  deriving newtype (Functor, Semigroup)

-- | Create a new 'Stack'.
--
-- @since 0.6.0
stack :: a -> Stack a
stack = Stack . one

-- | Push an element on to the head of the stack.
--
-- @since 0.5.0.0
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x `NonEmpty.cons` xs)

-- | Pop an element off the head of the stack.
--
-- @since 0.5.0.0
pop :: Stack a -> Stack a
pop = \case
  Stack (x :| []) -> Stack (x :| [])
  Stack (_ :| x : xs) -> Stack (x :| xs)

-- | Access the top of the stack.
--
-- @since 0.5.0.0
top :: Stack a -> a
top = unStack >>> NonEmpty.head

-- | Access the bottom of the stack.
--
-- @since 0.5.0.0
bottom :: Stack a -> a
bottom = unStack >>> NonEmpty.last

-- | The parent directory for the file at the head of the stack.
--
-- @since 0.5.0.0
directory ::
  Functor f =>
  -- | A function that maps a stack element to a 'FilePath'.
  (a -> f FilePath) ->
  -- | The stack.
  Stack a ->
  -- | The calculated directory.
  f FilePath
directory f = top >>> fmap takeDirectory . f

-- | Resolve a file path that is relative to the head of the stack.
--
-- @since 0.5.0.0
resolve ::
  Functor f =>
  -- | A function that maps a stack element to a 'FilePath'.
  (a -> f FilePath) ->
  -- | The file that needs to be resolved.
  FilePath ->
  -- | The stack to use for resolving.
  Stack a ->
  -- | Resolved file path.
  f FilePath
resolve f file stack = directory f stack <&> (`go` file)
  where
    go :: FilePath -> FilePath -> FilePath
    go dir file
      | isRelative file = normalise (dir </> file)
      | otherwise = normalise file
