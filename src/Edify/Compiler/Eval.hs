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
    emptyRuntime,
    depends,
    relativeToOutput,
    withInput,
    commandStatus,
    verifyCommand,
  )
where

import Control.Lens ((%=), (.=))
import qualified Edify.Compiler.Cycle as Cycle
import qualified Edify.Compiler.Error as Error
import qualified Edify.Compiler.Stack as Stack
import qualified Edify.System.FilePath as FilePath
import qualified Edify.System.Input as Input
import qualified Edify.Text.Fingerprint as Fingerprint

-- | Compiler evaluation state.
--
-- @since 0.5.0.0
data Runtime = Runtime
  { -- | Stack of files that we are currently processing.
    stack :: Stack.Stack FilePath,
    -- | Track cycles in dependencies.
    cycles :: Cycle.Deps FilePath,
    -- | Cache of commands that are approved for executing.
    fpcache :: Fingerprint.Cache Fingerprint.Commands
  }
  deriving stock (Generic, Show)

-- | Create an initial 'Runtime' value.
--
-- @since 0.5.0.0
emptyRuntime :: Runtime
emptyRuntime =
  Runtime
    { stack = mempty,
      cycles = Cycle.emptyDeps,
      fpcache = mempty
    }

-- | Record a dependency on the given input.
--
-- @since 0.5.0.0
depends ::
  forall m a.
  MonadIO m =>
  MonadState Runtime m =>
  -- | Input to add as a dependency.
  Input.Input ->
  -- | Continuation if an error occurs.
  (Error.Error -> m a) ->
  -- | Continuation if the dependency was added.
  (Maybe FilePath -> m a) ->
  -- | Final result.
  m a
depends input onerror onsuccess =
  case input of
    Input.FromFile file -> go file
    Input.FromHandle {} -> onsuccess Nothing
    Input.FromText {} -> onsuccess Nothing
  where
    go :: FilePath -> m a
    go file = do
      Runtime {..} <- get
      case Stack.top stack of
        Nothing -> onsuccess (Just file)
        Just top -> do
          full <- FilePath.makeAbsoluteToFile top file
          case Cycle.depends top full cycles of
            (Cycle.Cycles cs, _) ->
              onerror (Error.DependencyCycleError top file cs)
            (Cycle.NoCycles, deps) -> do
              #cycles .= deps
              onsuccess (Just full)

-- | Given an absolute path name, make it relative to file currently
-- being generated.  It is assumed that the file being generated can
-- be derived from the file at the bottom of the stack.
--
-- @since 0.5.0.0
relativeToOutput ::
  MonadIO m =>
  MonadState Runtime m =>
  -- | Input directory.
  FilePath ->
  -- | Output directory.
  FilePath ->
  -- | The path to translate.
  FilePath ->
  -- | The translated path.
  m FilePath
relativeToOutput input output path = do
  Runtime {stack} <- get
  case Stack.bottom stack of
    Nothing -> pure path
    Just file -> do
      let inoutdir = FilePath.toOutputPath input output file
      FilePath.makeRelativeToDir (FilePath.takeDirectory inoutdir) path

-- | Read input and pass it to a continuation, keeping track of
-- evaluation housekeeping (e.g., dependency tracking).
--
-- @since 0.5.0.0
withInput ::
  forall m a.
  MonadIO m =>
  MonadState Runtime m =>
  -- | The input to read.
  Input.Input ->
  -- | Continuation if an error occurs.
  (Error.Error -> m a) ->
  -- | Continuation if the input could be read.
  (Maybe FilePath -> LText -> m a) ->
  -- | Final result.
  m a
withInput input abort f = do
  depends input abort $ \path ->
    Input.readInput (maybe input Input.FromFile path)
      >>= either (abort . Error.InputError input) (go path)
  where
    go :: Maybe FilePath -> LText -> m a
    go path content =
      case path of
        Nothing -> f path content
        Just file -> do
          #stack %= Stack.push file
          result <- f path content
          #stack %= Stack.pop
          pure result

-- | Low-level access to command fingerprint status.
--
-- @since 0.5.0.0
commandStatus ::
  MonadIO m =>
  MonadState Runtime m =>
  -- | Directory where allow files are stored.
  FilePath ->
  -- | The command to verity.
  Text ->
  -- | Continuation to call if an error occurs.
  (Error.Error -> m a) ->
  -- | Continuation called with the file at the top of the stack, and
  -- the command fingerprint status.
  (FilePath -> Fingerprint.Status -> m a) ->
  -- | Final result.
  m a
commandStatus allowDir command onerror f = do
  Runtime {fpcache, stack} <- get
  case Stack.top stack of
    Nothing ->
      onerror (Error.InternalBugError "verifyCommand called on empty stack")
    Just top -> do
      (fp, cache) <- Fingerprint.read fpcache allowDir top
      #fpcache .= cache
      case Fingerprint.verify [command] . fold <$> fp of
        Nothing -> f top Fingerprint.Mismatch
        Just status -> f top status

-- | Verify that a command has been approved for executing.
--
-- @since 0.5.0.0
verifyCommand ::
  MonadIO m =>
  MonadState Runtime m =>
  -- | Directory where allow files are stored.
  FilePath ->
  -- | The command to verify.
  Text ->
  -- | Continuation if an error occurs.
  (Error.Error -> m a) ->
  -- | Continuation if the text was verified.
  (Text -> m a) ->
  -- | Final result.
  m a
verifyCommand allowDir command onerror onsuccess = do
  commandStatus allowDir command onerror $ \file status ->
    case status of
      Fingerprint.Mismatch ->
        onerror (Error.CommandBlockedError file command)
      Fingerprint.Verified ->
        onsuccess command
