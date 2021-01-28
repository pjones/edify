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
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Lazy.Builder as Builder
import qualified Development.Shake as Shake
import qualified Development.Shake.Database as Shake
import qualified Edify.Compiler.Asset as Asset
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
import qualified System.Directory as Directory

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
  Options.Options ->
  Project.Project ->
  Project.Target ->
  CommandSafety ->
  Asset.AssetMap ->
  Lang.Compiler a ->
  Eval Shake.Action a
eval options project target cmdmode assets = Free.iterM go
  where
    go :: Lang.CompilerF (Eval Shake.Action a) -> Eval Shake.Action a
    go = \case
      Lang.Tabstop k ->
        k (project ^. #projectTabstop)
      Lang.Asset file k ->
        Eval.depends (Input.FromFile file) abort $ \case
          Nothing -> do
            warn
              ( "internal error: failed to make path to asset absolute: "
                  <> toText file
              )
            k file
          Just path
            | HashMap.member (FilePath.takeExtension path) assets ->
              let indir = options ^. #optionsProjectDirectory
                  outdir = project ^. #projectOutputDirectory
                  ext = Asset.assetExtension (Project.targetFormat target)
                  output =
                    FilePath.toOutputPath indir outdir path
                      `FilePath.addExt` ext
               in lift (Shake.need [output]) >> k output
            | otherwise -> lift (Shake.need [path]) >> k path
      Lang.ReadInput input subexp k -> do
        x <- Eval.withInput input abort $ \path content -> do
          whenJust path (lift . Shake.need . one)
          eval options project target cmdmode assets (subexp content)
        k x
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

-- | Evaluate an asset compiler by converting it into a Shake action.
--
-- @since 0.5.0.0
assetEval ::
  -- | Project configuration.
  Project.Project ->
  -- | Asset compiler instructions.
  ((FilePath, FilePath) -> Asset.Asset) ->
  -- | The input and output files.
  (FilePath, FilePath) ->
  -- | Instructions converted to a Shake action.
  Shake.Action ()
assetEval project compiler (input, output) = do
  Shake.need [input]
  Free.iterM go (compiler (input, output))
  where
    go = \case
      Asset.SizeHints k ->
        let Project.SizeHints {..} = Project.projectSizeHints project
         in k (hintWidth, hintHeight)
      Asset.Command cmd args k ->
        exec [] cmd args >> k
      Asset.Shell cmd k ->
        exec [Shake.Shell] cmd [] >> k
      Asset.Rename old new k ->
        let forceOutputDir file =
              FilePath.takeDirectory output
                FilePath.</> FilePath.takeFileName file
         in liftIO
              ( Directory.renameFile
                  (forceOutputDir old)
                  (forceOutputDir new)
              )
              >> k

    exec :: [Shake.CmdOption] -> String -> [String] -> Shake.Action ()
    exec opts cmd args =
      let dir = FilePath.takeDirectory output
          fullOpts =
            [ Shake.Cwd dir,
              Shake.EchoStdout False,
              Shake.EchoStderr False
            ]
              <> opts
       in Shake.command_ fullOpts cmd args

-- | Generate Shake rules similar to 'Shake.%>' except this variant
-- will automatically discover the name of the input file and pass that
-- to the action.
--
-- @since 0.5.0.0
fileExtensionRule ::
  -- | Compiler options.
  Options.Options ->
  -- | The project that is currently active.
  Project.Project ->
  -- | A pair of file extensions.  The first is for the input file and
  -- the second is for the output file.
  (FilePath.InputExt, FilePath.Ext) ->
  -- | A function that takes the name of the source file, the name of
  -- the output file, and then produces a Shake action.
  (FilePath -> FilePath -> Shake.Action ()) ->
  -- | The generated Shake rules.
  Shake.Rules ()
fileExtensionRule options project (inputExt, extOut) f =
  let dir = project ^. #projectOutputDirectory
      extIn = case inputExt of
        FilePath.FromProjectInput x -> x
        FilePath.FromBuildProduct _ x -> x
      filePattern =
        dir <> "//*"
          <> FilePath.fromExt extIn
          <> FilePath.fromExt extOut
   in filePattern Shake.%> \output -> do
        let indir = options ^. #optionsProjectDirectory
            outdir = project ^. #projectOutputDirectory
            input = FilePath.toInputPathViaInputExt indir outdir inputExt output
        f input output

-- | Generate Shake rules for each asset compiler listed in the 'Asset.AssetMap'.
--
-- @since 0.5.0.0
assetRules ::
  -- | Compiler options.
  Options.Options ->
  -- | Project configuration.
  Project.Project ->
  -- | The map of asset compilers.
  Asset.AssetMap ->
  -- | Generated Shake rules.
  Shake.Rules ()
assetRules options project =
  hmFoldMapWithKey $ \ext compilers ->
    for_ (enumFrom minBound) $ \format ->
      let extIn = FilePath.FromProjectInput ext
          extOut = Asset.assetExtension format
          compiler = curry $ assetEval project (compilers format)
       in fileExtensionRule options project (extIn, extOut) compiler
  where
    -- HashMap.foldMapWithKey was introduced in 0.2.11
    hmFoldMapWithKey f = HashMap.foldrWithKey (\k v x -> x <> f k v) mempty

-- | Shake rule for processing and removing Edify features from a
-- Markdown document, producing the target Markdown.
--
-- Example:
--
--   Input File: content/foo.md
--   Output File: build/content/foo.md.slides
--
-- @since 0.5.0.0
markdownRule ::
  Options.Options ->
  Project.Project ->
  Project.Target ->
  CommandSafety ->
  Asset.AssetMap ->
  Shake.Rules ()
markdownRule options project target cmdmode assets =
  let extIn = FilePath.FromProjectInput (FilePath.Ext "md")
      extOut = Project.targetFileExtension target
   in fileExtensionRule options project (extIn, extOut) $ \input output -> do
        let compiler = Markdown.compile (Input.FromFile input)
        markdown <-
          eval options project target cmdmode assets compiler
            & evaluatingStateT Eval.emptyRuntime
        writeFileLText
          output
          ( Markdown.markdownAST markdown
              & AST.markdownT
              & Builder.toLazyText
          )

-- | Convert a target's command into a Shake rule.
--
-- Example:
--
--   Input File: build/content/foo.md.slides
--   Output File: build/content/foo.slides.pdf
--
-- @since 0.5.0.0
targetRule ::
  Options.Options ->
  Project.Project ->
  Project.Target ->
  Shake.Rules ()
targetRule options project target =
  let extIn =
        FilePath.FromBuildProduct
          (FilePath.Ext "md")
          (Project.targetFileExtension target)
      extOut = Project.formatExtension (Project.targetFormat target)
      eval = curry $ assetEval project compiler
   in fileExtensionRule options project (extIn, extOut) eval
  where
    compiler :: (FilePath, FilePath) -> Asset.Asset
    compiler = Asset.shell . toString . Project.targetCommand target

-- | All Shake rules for building an entire project.
--
-- @since 0.5.0.0
rules ::
  Options.Options ->
  Project.Project ->
  CommandSafety ->
  Asset.AssetMap ->
  Shake.Rules ()
rules options project cmdmode assets = do
  assetRules options project assets

  for_ (project ^. #projectInputFiles) $ \file ->
    for_ (project ^. #projectTargets) $ \target -> do
      let indir = options ^. #optionsProjectDirectory
          outdir = project ^. #projectOutputDirectory
          targetE = Project.targetFileExtension target
          format = Project.formatExtension (Project.targetFormat target)
          output =
            FilePath.toOutputPath indir outdir file
              `FilePath.replaceExt` (targetE <> format)
      markdownRule options project target cmdmode assets
      targetRule options project target
      Shake.want [output]

-- | Build an Edify project.
--
-- @since 0.5.0.0
main ::
  Options.Options ->
  Project.Project ->
  CommandSafety ->
  IO ()
main options project cmdmode = do
  let assets = Asset.assets
  (_, after) <- Shake.shakeWithDatabase
    shakeOptions
    (rules options project cmdmode assets)
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
