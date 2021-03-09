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
    tabstop,
    unwantedDivClasses,
    asset,
    withFileContents,
    exec,
    abort,
    CompilerF (..),

    -- * Re-exports
    Error (..),
  )
where

import Control.Monad.Free.Church (F, MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import qualified Data.CaseInsensitive as CaseInsensitive
import Edify.Compiler.Error (Error (..))
import qualified Edify.Markdown.Attributes as Attrs
import Edify.System.Input (Input)
import qualified Edify.Text.Indent as Indent
import Edify.Text.Narrow (Token)

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
  -- | Access the 'Indent.Tabstop' that is currently in effect.
  Tabstop :: (Indent.Tabstop -> k) -> CompilerF k
  -- | The set of classes for divs that need to be removed.
  UnwantedDivClasses ::
    (HashSet (CaseInsensitive.CI Attrs.CssIdent) -> k) ->
    CompilerF k
  -- | Resolve the path to an asset.  If the asset needs to be
  -- compiled the path to the final build result is returned.
  -- Otherwise the absolute path to the source asset is returned.
  Asset :: FilePath -> (FilePath -> k) -> CompilerF k
  -- | Pass the contents of the given file to the continuation.  If a
  -- token is given, narrow the contents of the file first.
  WithFileContents ::
    forall a k.
    -- The input to read from.
    FilePath ->
    -- Optionally narrow the input.
    Maybe Token ->
    -- A function to process the read data.
    (LText -> Compiler a) ->
    -- The result of the processing function.
    (a -> k) ->
    -- Constructed value.
    CompilerF k
  -- | Execute a shell command feeding it some input.
  Exec :: (Command, StandardInput) -> (Text -> k) -> CompilerF k
  -- | Abort the compilation with the given error.
  Abort :: (Input -> Error) -> CompilerF k

instance Functor CompilerF where
  fmap f = \case
    Tabstop g -> Tabstop (f . g)
    UnwantedDivClasses g -> UnwantedDivClasses (f . g)
    Asset file g -> Asset file (f . g)
    WithFileContents file token h g -> WithFileContents file token h (f . g)
    Exec x g -> Exec x (f . g)
    Abort e -> Abort e

-- | Free monad wrapper for 'CompilerF'.
--
-- @since 0.5.0.0
type Compiler = F CompilerF

makeFree ''CompilerF
