{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

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
-- Domain specific language for compiling markdown documents.
module Edify.Compiler.Lang
  ( Compiler,
    Command,
    StandardInput,
    options,
    readInput,
    exec,
    abort,
    CompilerF (..),

    -- * Re-exports
    Error (..),
  )
where

import Control.Monad.Free.Church (F, MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import Edify.Compiler.Error (Error (..))
import qualified Edify.Compiler.Options as Options
import Edify.Input (Input)

-- | Shell commands.
--
-- @since 0.5.0.0
type Command = Text

-- | Input to feed to a shell command.
--
-- @since 0.5.0.0
type StandardInput = Text

-- | Primary operations in the @Compiler@ language.
--
-- @since 0.5.0.0
data CompilerF k where
  -- | Access the options provided to the compiler.
  Options :: (Options.Options -> k) -> CompilerF k
  -- | Load text from the given input source.
  ReadInput :: forall a k. Input -> (LText -> Compiler a) -> (a -> k) -> CompilerF k
  -- | Execute a shell command feeding it some input.
  Exec :: (Command, StandardInput) -> (Text -> k) -> CompilerF k
  -- | Abort the compilation with the given error.
  Abort :: Error -> CompilerF k

instance Functor CompilerF where
  fmap f = \case
    Options g -> Options (f . g)
    ReadInput x y g -> ReadInput x y (f . g)
    Exec x g -> Exec x (f . g)
    Abort e -> Abort e

-- | Free monad wrapper for 'CompilerF'.
--
-- @since 0.5.0.0
type Compiler = F CompilerF

makeFree ''CompilerF
