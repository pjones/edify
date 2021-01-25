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
-- Interpreter that produces rules for the Shake build system.
module Edify.Compiler.Shake
  ( CommandSafety (..),
    eval,
    rules,
    main,
  )
where

import Control.Lens ((^.))
import qualified Control.Monad.Free.Church as Free
import qualified Data.Text.Lazy.Builder as Builder
import qualified Development.Shake as Shake
import qualified Development.Shake.Database as Shake
import qualified Edify.Compiler.Error as Error
import Edify.Compiler.Eval (Eval)
import qualified Edify.Compiler.Eval as Eval
import qualified Edify.Compiler.FilePath as FilePath
import qualified Edify.Compiler.Lang as Lang
import qualified Edify.Compiler.Markdown as Markdown
import qualified Edify.Compiler.Options as Options
import qualified Edify.Compiler.Project as Project
import qualified Edify.Compiler.Stack as Stack
import qualified Edify.Input as Input
import qualified Edify.Markdown.AST as AST

-- | Safety controls around running shell commands.
--
-- @since 0.5.0.0
data CommandSafety
  = RequireCommandFingerprints
  | UnsafeAllowAllCommands

-- | General purpose interpreter that results in a Shake action.
--
-- @since 0.5.0.0
eval ::
  Options.OptionsF Identity ->
  Project.ProjectF Identity ->
  CommandSafety ->
  Lang.Compiler a ->
  Eval Shake.Action a
eval options project cmdmode = Free.iterM go
  where
    go :: Lang.CompilerF (Eval Shake.Action a) -> Eval Shake.Action a
    go = \case
      Lang.Tabstop k ->
        k (project ^. #projectTabstop)
      Lang.ReadInput input subexp k -> do
        Eval.withInput input abort (const (eval options project cmdmode . subexp)) >>= k
      Lang.Exec (pending, input) k ->
        verifyCommandWithBypass pending $ \approved -> do
          dir <- gets Eval.stack >>= Stack.directory
          let copts =
                [ Shake.Cwd dir,
                  Shake.StdinBS $ encodeUtf8 input,
                  Shake.Shell
                ]
          Shake.Stdout (output :: LByteString) <-
            lift $ Shake.command copts (toString approved) []
          k (decodeUtf8 output)
      Lang.Abort e ->
        abort e

    warn :: Text -> Eval Shake.Action ()
    warn = toString >>> Shake.putWarn >>> lift

    abort :: Error.Error -> Eval Shake.Action a
    abort e = do
      Eval.Runtime {stack} <- get
      let str = Error.renderError e <> "\n" <> show stack
      lift $ do
        Shake.putError str
        fail str

    -- Verify a command fingerprint then call the given continuation.
    verifyCommandWithBypass ::
      -- | Command to verify.
      Text ->
      -- | Continuation to call if command is allowed.
      (Text -> Eval Shake.Action a) ->
      Eval Shake.Action a
    verifyCommandWithBypass command f =
      case cmdmode of
        RequireCommandFingerprints ->
          Eval.verifyCommand options command abort f
        UnsafeAllowAllCommands -> do
          warn ("Command forced due to --unsafe-allow-commands: " <> command)
          f command

-- | Generate Shake rules similar to 'Shake.%>' except this variant
-- will automatically discover the name of the input file and pass that
-- to the action.
--
-- @since 0.5.0.0
fileExtensionRule ::
  -- | Compiler options.
  Options.OptionsF Identity ->
  -- | The project that is currently active.
  Project.ProjectF Identity ->
  -- | A pair of file extensions.  The first is for the input file and
  -- the second is for the output file.
  (String, String) ->
  -- | A function that takes the name of the source file, the name of
  -- the output file, and then produces a Shake action.
  (FilePath -> FilePath -> Shake.Action ()) ->
  -- | The generated Shake rules.
  Shake.Rules ()
fileExtensionRule options project (extIn, extOut) f =
  let dir = project ^. #projectOutputDirectory
      filePattern =
        dir <> "//*"
          <> extIn -- FIXME: Ensure these are formatted properly.
          <> extOut -- FIXME: probably use a newtype for extensions.
   in filePattern Shake.%> \output -> do
        let input = FilePath.toInputName options project output
        f input output

-- | Shake rule for processing and removing Edify features from a
-- Markdown document, producing the target Markdown.
--
-- @since 0.5.0.0
markdownRule ::
  Options.OptionsF Identity ->
  Project.ProjectF Identity ->
  CommandSafety ->
  Shake.Rules ()
markdownRule options project cmdmode =
  fileExtensionRule options project (".md", ".edify") $ \input output -> do
    markdown <-
      eval options project cmdmode (Markdown.compile (Input.FromFile input))
        & evaluatingStateT Eval.emptyRuntime
    writeFileLText
      output
      ( Markdown.markdownAST markdown
          & AST.markdownT
          & Builder.toLazyText
      )

-- | All Shake rules for building an entire project.
--
-- @since 0.5.0.0
rules ::
  Options.OptionsF Identity ->
  Project.ProjectF Identity ->
  CommandSafety ->
  Shake.Rules ()
rules options project cmdmode = do
  markdownRule options project cmdmode

  forM_ (project ^. #projectInputFiles) $
    \file -> do
      let md = FilePath.toOutputName options project file "edify"
      Shake.want [md]

-- | Build an Edify project.
--
-- @since 0.5.0.0
main ::
  Options.OptionsF Identity ->
  Project.ProjectF Identity ->
  CommandSafety ->
  IO ()
main options project cmdmode = do
  (_, after) <- Shake.shakeWithDatabase
    shakeOptions
    (rules options project cmdmode)
    $ \db -> do
      Shake.shakeOneShotDatabase db
      Shake.shakeRunDatabase db []
  Shake.shakeRunAfter shakeOptions after
  where
    shakeOptions :: Shake.ShakeOptions
    shakeOptions =
      Shake.shakeOptions
        { Shake.shakeFiles = project ^. #projectOutputDirectory
        }
