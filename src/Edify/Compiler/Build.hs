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
module Edify.Compiler.Build
  ( -- * Markdown Conversion
    convert,

    -- * Executing Shell Commands
    execIfApproved,
    unsafeRunCommand,

    -- * Re-exports
    Input.Input (..),
    Indent.Tabstop (..),
    Indent.defaultTabstop,
  )
where

import Control.Exception (throwIO)
import Control.Monad.Except (MonadError, throwError)
import qualified Control.Monad.Free.Church as Free
import qualified Data.Text.Lazy.Builder as Builder
import qualified Edify.Compiler.Eval as Eval
import qualified Edify.Compiler.Lang as Lang
import qualified Edify.Compiler.Markdown as Markdown
import qualified Edify.Compiler.Stack as Stack
import qualified Edify.Markdown.AST as AST
import qualified Edify.System.FilePath as FilePath
import qualified Edify.System.Input as Input
import qualified Edify.Text.Indent as Indent
import qualified System.Directory as Directory
import qualified System.Process as Process

-- | Evaluate a 'Lang.Compiler'.
eval ::
  forall m a.
  MonadIO m =>
  MonadError Lang.Error m =>
  MonadState Eval.Runtime m =>
  -- | Control tabstop expansion while indenting code blocks.
  Indent.Tabstop ->
  -- | Function to rewrite an asset file name.
  (FilePath -> m FilePath) ->
  -- | Function to execute shell commands.
  (FilePath -> (Lang.Command, Lang.StandardInput) -> m Text) ->
  -- | The 'Lang.Compiler' to evaluate.
  Lang.Compiler a ->
  m a
eval tabstop assetHandler execHandler =
  Free.iterM go
  where
    eval' :: Lang.Compiler b -> m b
    eval' = eval tabstop assetHandler execHandler

    go :: Lang.CompilerF (m a) -> m a
    go = \case
      Lang.Tabstop k ->
        k tabstop
      Lang.UnwantedDivClasses k ->
        k mempty
      Lang.Asset file k ->
        assetHandler file >>= k
      Lang.WithFileContents file token subexp k ->
        Eval.withFileContents file token (const $ eval' . subexp) >>= k
      Lang.Exec command k -> do
        top <- gets Eval.stack <&> Stack.top

        file <-
          Input.toFilePath top
            & maybe (liftIO Directory.getCurrentDirectory) pure

        execHandler file command >>= k
      Lang.Abort f -> do
        top <- gets Eval.stack <&> Stack.top
        throwError (f top)

-- | Convert a Markdown file that uses Edify extensions into a
-- Markdown file with those extensions evaluated and removed.
--
-- @since 0.6.0
convert ::
  MonadIO m =>
  -- | The 'Input.Input' to process.
  Input.Input ->
  -- | Control tabstop expansion while indenting code blocks.
  --
  -- Use 'Indent.defaultTabstop' if you don't care about this.
  Indent.Tabstop ->
  -- | Function to rewrite an asset file names.
  --
  -- This function is given the file name of an asset mentioned in the
  -- input Markdown document and should return a new file name to put
  -- in the output document.
  (FilePath -> m FilePath) ->
  -- | Function to execute shell commands.
  --
  -- This function is given a command to execute along with its
  -- standard input.  It should return the result (standard output) of
  -- the command.
  --
  -- NOTE: There are no safety or security precautions taken here.
  -- It's up to the caller to only execute commands that are safe.
  -- Consider using 'execIfApproved' as this function.
  (FilePath -> (Lang.Command, Lang.StandardInput) -> m Text) ->
  -- | The final markdown document or an error.
  m (Either Lang.Error LText)
convert input tabstop assetHandler execHandler = runExceptT $ do
  contents <-
    Input.readInput input
      >>= either (throwError . (`Lang.InputError` input)) pure

  let compiler = Markdown.compile contents
      lift' = lift . lift
      assetM = lift' . assetHandler
      execM = (lift' .) . execHandler

  eval tabstop assetM execM compiler
    & evaluatingStateT (Eval.emptyRuntime input)
    <&> (AST.markdownT >>> Builder.toLazyText)

-- | Execute a command if it was previously approved by @edify allow@.
--
-- NOTE: This function will block until the command has completed and
-- will read /all/ of its standard output into memory.
--
-- @since 0.6.0
execIfApproved ::
  forall m.
  MonadIO m =>
  -- | A directory where command approval files (fingerprints) are stored.
  FilePath ->
  -- | The absolute path to the Markdown file that is requesting that
  -- a command be executed.
  FilePath ->
  -- | The command to execute.
  (Lang.Command, Lang.StandardInput) ->
  -- | The output of the command.
  m Text
execIfApproved dir from (command, input) =
  let exec = lift . unsafeRunCommand from . (,input)
   in Eval.verifyCommand dir command (liftIO . throwIO) exec
        & evaluatingStateT (Eval.emptyRuntime (Input.FromFile from))

-- | Executes a shell command with the given text as its standard
-- input.  Returns the standard output from the command.
--
-- NOTE: This function will run any command you give it.  No safety or
-- security features are employed.  Consider using 'execIfApproved'
-- instead.
--
-- NOTE: This function will block until the command has completed and
-- will read /all/ of its standard output into memory.
--
-- @since 0.6.0
unsafeRunCommand ::
  MonadIO m =>
  -- | The Markdown file that is requesting the shell command
  -- execution.  The shell command will be run within the directory
  -- containing the Markdown file.
  FilePath ->
  -- | The command to run along with its standard input.
  (Lang.Command, Lang.StandardInput) ->
  -- | The output of the command.
  m Text
unsafeRunCommand file (cmd, input) = liftIO $ do
  let process =
        (Process.shell (toString cmd))
          { Process.cwd = Just (FilePath.takeDirectory file)
          }
  out <- Process.readCreateProcess process (toString input)
  pure (toText out)
