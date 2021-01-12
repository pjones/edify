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
data CompilerF k
  = -- | Access the options provided to the compiler.
    Options (Options.Options -> k)
  | -- | Load text from the given input source.
    ReadInput Input (LText -> k)
  | -- | Execute a shell command feeding it some input.
    Exec (Command, StandardInput) (Text -> k)
  | -- | Abort the compilation with the given error.
    Abort Error
  deriving stock (Functor)

-- | Free monad wrapper for 'CompilerF'.
--
-- @since 0.5.0.0
type Compiler = F CompilerF

makeFree ''CompilerF
