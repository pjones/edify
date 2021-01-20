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
  ( eval,
    rules,
    main,
  )
where

import Control.Lens ((%=), (^.))
import qualified Control.Monad.Free.Church as Free
import qualified Data.Text.Lazy.Builder as Builder
import qualified Development.Shake as Shake
import qualified Development.Shake.Database as Shake
import qualified Edify.Compiler.Cycle as Cycle
import qualified Edify.Compiler.Error as Error
import Edify.Compiler.Eval
import qualified Edify.Compiler.FilePath as FilePath
import qualified Edify.Compiler.Lang as Lang
import qualified Edify.Compiler.Markdown as Markdown
import qualified Edify.Compiler.Options as Options
import qualified Edify.Compiler.Stack as Stack
import qualified Edify.Input as Input
import qualified Edify.Markdown.AST as AST

-- | General purpose interpreter that results in a Shake action.
--
-- @since 0.5.0.0
eval ::
  Options.Options ->
  Lang.Compiler a ->
  Eval Shake.Action a
eval options = Free.iterM go
  where
    go :: Lang.CompilerF (Eval Shake.Action a) -> Eval Shake.Action a
    go = \case
      Lang.Options k ->
        k options
      Lang.ReadInput input subexp k ->
        depends input abort $ \path -> do
          content <-
            Input.readInput (maybe input Input.FromFile path)
              >>= either (abort . Lang.InputError input) pure
          result <- case path of
            Nothing -> eval options (subexp content)
            Just file -> do
              lift (Shake.need [file])
              #stack %= Stack.push file
              result <- eval options (subexp content)
              #stack %= Stack.pop
              pure result
          k result
      Lang.Exec (pending, input) k ->
        verifyCommand options pending warn abort $ \approved -> do
          dir <- gets stack >>= Stack.directory
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
      Runtime {stack} <- get
      let str = Error.renderError e <> "\n" <> show stack
      lift $ do
        Shake.putError str
        fail str

-- | Generate Shake rules similar to 'Shake.%>' except this variant
-- will automatically discover the name of the input file and pass that
-- to the action.
--
-- @since 0.5.0.0
fileExtensionRule ::
  -- | Compiler options.
  Options.Options ->
  -- | A pair of file extensions.  The first is for the input file and
  -- the second is for the output file.
  (String, String) ->
  -- | A function that takes the name of the source file, the name of
  -- the output file, and then produces a Shake action.
  (FilePath -> FilePath -> Shake.Action ()) ->
  -- | The generated Shake rules.
  Shake.Rules ()
fileExtensionRule options (extIn, extOut) f =
  let dir = options ^. #optionsProjectConfig . #projectOutputDirectory
      filePattern =
        dir <> "//*"
          <> extIn -- FIXME: Ensure these are formatted properly.
          <> extOut -- FIXME: probably use a newtype for extensions.
   in filePattern Shake.%> \output -> do
        let input = FilePath.toInputName options output
        f input output

-- | Shake rule for processing and removing Edify features from a
-- Markdown document, producing the target Markdown.
--
-- @since 0.5.0.0
markdownRule :: Options.Options -> Shake.Rules ()
markdownRule options =
  fileExtensionRule options (".md", ".edify") $ \input output -> do
    markdown <-
      eval options (Markdown.compile (Input.FromFile input))
        & evaluatingStateT runtime
    writeFileLText
      output
      ( Markdown.markdownAST markdown
          & AST.markdownT
          & Builder.toLazyText
      )
  where
    runtime :: Runtime
    runtime =
      Runtime
        { stack = mempty,
          cycles = Cycle.emptyDeps,
          fpcache = mempty
        }

-- | All Shake rules for building an entire project.
--
-- @since 0.5.0.0
rules :: Options.Options -> Shake.Rules ()
rules options = do
  markdownRule options

  forM_ (options ^. #optionsProjectConfig . #projectInputFiles) $
    \file -> do
      let md = FilePath.toOutputName options file "edify"
      Shake.want [md]

-- | Build an Edify project.
--
-- @since 0.5.0.0
main :: Options.Options -> IO ()
main options = do
  (_, after) <- Shake.shakeWithDatabase
    shakeOptions
    (rules options)
    $ \db -> do
      Shake.shakeOneShotDatabase db
      Shake.shakeRunDatabase db []
  Shake.shakeRunAfter shakeOptions after
  where
    shakeOptions :: Shake.ShakeOptions
    shakeOptions =
      Shake.shakeOptions
        { Shake.shakeFiles =
            options
              ^. #optionsProjectConfig . #projectOutputDirectory
        }
