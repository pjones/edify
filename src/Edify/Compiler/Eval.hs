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
    withFileContents,
    commandStatus,
    verifyCommand,
  )
where

import Control.Lens ((%=), (.=))
import Control.Monad.Except (MonadError, throwError)
import qualified Edify.Compiler.Cycle as Cycle
import qualified Edify.Compiler.Error as Error
import Edify.Compiler.Stack (Stack)
import qualified Edify.Compiler.Stack as Stack
import qualified Edify.System.FilePath as FilePath
import Edify.System.Input (Input)
import qualified Edify.System.Input as Input
import qualified Edify.Text.Fingerprint as Fingerprint
import qualified Edify.Text.Format as Format
import Edify.Text.Narrow (Token)
import qualified System.Directory as Directory

-- | Compiler evaluation state.
--
-- @since 0.5.0.0
data Runtime = Runtime
  { -- | Stack of inputs that we are currently processing.
    stack :: Stack Input,
    -- | Track cycles in dependencies.
    cycles :: Cycle.Deps FilePath,
    -- | Cache of commands that are approved for executing.
    fpcache :: Fingerprint.Cache Fingerprint.Commands
  }
  deriving stock (Generic, Show)

-- | Create an initial 'Runtime' value.
--
-- @since 0.5.0.0
emptyRuntime :: Input -> Runtime
emptyRuntime initialInput =
  Runtime
    { stack = Stack.stack initialInput,
      cycles = Cycle.emptyDeps,
      fpcache = mempty
    }

-- | Record a dependency on the given file.
--
-- @since 0.6.0
depends ::
  forall m.
  MonadIO m =>
  MonadError Error.Error m =>
  MonadState Runtime m =>
  -- | The file that is being added as a dependency.
  FilePath ->
  -- | The absolute path to the new dependency.
  m FilePath
depends file = do
  Runtime {..} <- get
  case Input.toFilePath (Stack.top stack) of
    Nothing -> liftIO (Directory.makeAbsolute file)
    Just top -> do
      full <- FilePath.makeAbsoluteToFile top file
      case Cycle.depends top full cycles of
        (Cycle.Cycles cs, _) ->
          throwError (Error.DependencyCycleError top file cs)
        (Cycle.NoCycles, deps) -> do
          #cycles .= deps
          pure full

-- | Given an absolute path name, make it relative to the file
-- currently being generated.
--
-- It is assumed that the file being generated can be derived from the
-- file at the bottom of the stack.  If the bottom of the stack isn't
-- a 'FilePath' the file will be make relative to the output
-- directory.
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
  let dir =
        Stack.bottom stack
          & Input.toFilePath
          & fmap (FilePath.toOutputPath input output)
          & fmap FilePath.takeDirectory
          & fromMaybe output
  FilePath.makeRelativeToDir dir path

-- | Read a file and pass its contents to a continuation, keeping
-- track of evaluation housekeeping (e.g., dependency tracking).
--
-- @since 0.6.0
withFileContents ::
  forall m a.
  MonadIO m =>
  MonadError Error.Error m =>
  MonadState Runtime m =>
  -- | The file to read.
  FilePath ->
  -- | Token to narrow input to.
  Maybe Token ->
  -- | Continuation if the file could be read.
  (FilePath -> LText -> m a) ->
  -- | Final result.
  m a
withFileContents = dependThenGo
  where
    -- Hack to prevent the go function from seeing the original file name.
    dependThenGo :: FilePath -> Maybe Token -> (FilePath -> LText -> m a) -> m a
    dependThenGo file token f = depends file >>= \file' -> go file' token f

    go :: FilePath -> Maybe Token -> (FilePath -> LText -> m a) -> m a
    go file token f = do
      let input = Input.FromFile file
      content <-
        Input.readInput input
          >>= either (throwError . (`Error.InputError` input)) pure
      narrowed <-
        case token of
          Nothing ->
            pure content
          Just t ->
            Format.narrow (Format.fromInput input) t content
              & either (throwError . (`Error.FormatError` input)) pure

      #stack %= Stack.push input
      result <- f file narrowed
      #stack %= Stack.pop
      pure result

-- | Low-level access to command fingerprint status.
--
-- @since 0.6.0
commandStatus ::
  MonadIO m =>
  MonadState Runtime m =>
  -- | Directory where allow files are stored.
  FilePath ->
  -- | The command to verity.
  Text ->
  -- | Continuation called with the file at the top of the stack, and
  -- the command fingerprint status.
  (FilePath -> Fingerprint.Status -> m a) ->
  -- | Final result.
  m a
commandStatus allowDir command f = do
  Runtime {fpcache, stack} <- get
  currentFile <-
    Stack.top stack
      & Input.toFilePath
      & maybe (liftIO Directory.getCurrentDirectory) pure

  (fp, cache) <- Fingerprint.read fpcache allowDir currentFile
  #fpcache .= cache

  case Fingerprint.verify [command] . fold <$> fp of
    Nothing -> f currentFile Fingerprint.Mismatch
    Just status -> f currentFile status

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
  commandStatus allowDir command $ \file status ->
    case status of
      Fingerprint.Mismatch ->
        onerror (Error.CommandBlockedError file command)
      Fingerprint.Verified ->
        onsuccess command
