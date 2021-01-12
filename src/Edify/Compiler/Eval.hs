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
-- General purpose evaluation utilities.
module Edify.Compiler.Eval
  ( Runtime (..),
    Eval,
    depends,
    verifyFingerprint,
  )
where

import Control.Lens ((.=))
import qualified Edify.Compiler.Cycle as Cycle
import qualified Edify.Compiler.Error as Error
import qualified Edify.Compiler.FilePath as FilePath
import qualified Edify.Compiler.Lang as Lang
import qualified Edify.Compiler.Options as Options
import qualified Edify.Compiler.Stack as Stack
import qualified Edify.Input as Input

-- | FIXME: Write documentation for Runtime
--
-- @since 0.5.0.0
data Runtime = Runtime
  { stack :: Stack.Stack FilePath,
    cycles :: Cycle.Deps FilePath
  }
  deriving stock (Generic, Show)

-- | FIXME: Write documentation for Eval a
--
-- @since 0.5.0.0
type Eval = StateT Runtime

-- | Record a dependency on the given input.
--
-- @since 0.5.0.0
depends ::
  forall m a.
  Monad m =>
  -- | Input to add as a dependency.
  Input.Input ->
  -- | Continuation if an error occurs.
  (Error.Error -> Eval m a) ->
  -- | Continuation if the dependency was added.
  (Maybe FilePath -> Eval m a) ->
  -- | Final result.
  Eval m a
depends input onerror onsuccess =
  case input of
    Input.FromFile file -> go file
    Input.FromHandle {} -> onsuccess Nothing
    Input.FromText {} -> onsuccess Nothing
  where
    go :: FilePath -> Eval m a
    go file = do
      Runtime {..} <- get
      case Stack.top stack of
        Nothing -> onsuccess (Just file)
        Just top ->
          let full = FilePath.makeAbsoluteTo top file
           in case Cycle.depends top file cycles of
                (Cycle.Cycles cs, _) ->
                  onerror (Error.DependencyCycleError top file cs)
                (Cycle.NoCycles, deps) -> do
                  #cycles .= deps
                  onsuccess (Just full)

-- | FIXME: Write description for verifyFingerprint
--
-- @since 0.5.0.0
verifyFingerprint ::
  -- | Compiler options.
  Options.Options ->
  -- | The text to verify.
  Text ->
  -- | Function to output warnings.
  (Text -> Eval m ()) ->
  -- | Continuation if an error occurs.
  (Lang.Error -> Eval m a) ->
  -- | Continuation if the text was verified.
  Eval m a ->
  -- | Final result.
  Eval m a
verifyFingerprint = undefined
