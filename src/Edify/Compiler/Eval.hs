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
    verifyCommand,
  )
where

import Control.Lens ((.=), (^.))
import qualified Edify.Compiler.Cycle as Cycle
import qualified Edify.Compiler.Error as Error
import qualified Edify.Compiler.FilePath as FilePath
import qualified Edify.Compiler.Fingerprint as Fingerprint
import qualified Edify.Compiler.Options as Options
import qualified Edify.Compiler.Stack as Stack
import qualified Edify.Input as Input

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

-- | Compiler evaluation type.
--
-- @since 0.5.0.0
type Eval = StateT Runtime

-- | Record a dependency on the given input.
--
-- @since 0.5.0.0
depends ::
  forall m a.
  MonadIO m =>
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
        Just top -> do
          full <- FilePath.makeAbsoluteTo top file
          case Cycle.depends top full cycles of
            (Cycle.Cycles cs, _) ->
              onerror (Error.DependencyCycleError top file cs)
            (Cycle.NoCycles, deps) -> do
              #cycles .= deps
              onsuccess (Just full)

-- | Verify that a command has been approved for executing.
--
-- @since 0.5.0.0
verifyCommand ::
  MonadIO m =>
  -- | Compiler options.
  Options.Options ->
  -- | The command to verify.
  Text ->
  -- | Function to output warnings.
  (Text -> Eval m ()) ->
  -- | Continuation if an error occurs.
  (Error.Error -> Eval m a) ->
  -- | Continuation if the text was verified.
  (Text -> Eval m a) ->
  -- | Final result.
  Eval m a
verifyCommand options command onwarn onerror onsuccess
  | options ^. #optionsUnsafeAllowCommands = do
    onwarn ("Command forced due to --unsafe-allow-commands: " <> command)
    onsuccess command
  | otherwise = do
    Runtime {fpcache, stack} <- get
    case Stack.top stack of
      Nothing ->
        onerror (Error.InternalBugError "verifyCommand called on empty stack")
      Just top -> do
        let allowDir = options ^. #optionsUserConfig . #userCommandAllowDir
        (fp, cache) <- Fingerprint.read fpcache allowDir top
        #fpcache .= cache
        case Fingerprint.verify command . fold <$> fp of
          Nothing ->
            onerror (Error.CommandBlockedError top command)
          Just Fingerprint.Mismatch ->
            onerror (Error.CommandBlockedError top command)
          Just Fingerprint.Verified ->
            onsuccess command
