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
  ( Options (..),
    CommandSafety (..),
    eval,
    rules,
    main,
  )
where

import Control.Exception (catch, throwIO)
import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import qualified Control.Monad.Free.Church as Free
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.IO.Utf8 as Utf8
import qualified Development.Shake as Shake
import qualified Development.Shake.Database as Shake
import qualified Edify.Compiler.Asset as Asset
import qualified Edify.Compiler.Error as Error
import qualified Edify.Compiler.Eval as Eval
import qualified Edify.Compiler.Lang as Lang
import qualified Edify.Compiler.Markdown as Markdown
import qualified Edify.Compiler.Stack as Stack
import qualified Edify.Compiler.User as User
import qualified Edify.Markdown.AST as AST
import qualified Edify.Project as Project
import qualified Edify.System.Exit as Exit
import qualified Edify.System.FilePath as FilePath
import qualified Edify.System.Input as Input
import qualified Edify.Text.Format as Format
import qualified Edify.Text.Pretty as P
import qualified GHC.Conc as GHC
import qualified System.Directory as Directory
import qualified System.FSNotify as FSNotify

-- | Safety controls around running shell commands.
--
-- @since 0.5.0.0
data CommandSafety
  = RequireCommandFingerprints
  | UnsafeAllowAllCommands

-- | Options that control Shake builds.
--
-- @since 0.5.0.0
data Options = Options
  { -- | How to deal with shell commands and fingerprints.
    optionsCommandSafety :: CommandSafety,
    -- | How many threads to use (parallel jobs).  A value of @0@
    -- means to use half of the available CPU cores.
    optionsThreads :: Int,
    -- | If 'True', automatically rebuild when files are changed.
    optionsWatch :: Bool
  }

-- | Transformer stack for evaluating the 'Lang.Compiler' EDSL.
--
-- @since 0.5.0.0
type Eval = ExceptT Lang.Error (StateT Eval.Runtime Shake.Action)

-- | Lift a Shake action.
--
-- @since 0.5.0.0
shake :: Shake.Action a -> Eval a
shake = lift . lift

-- | General purpose interpreter that results in a Shake action.
--
-- @since 0.5.0.0
eval ::
  User.User ->
  Project.Project ->
  Project.Target ->
  CommandSafety ->
  Asset.AssetMap ->
  Lang.Compiler a ->
  Eval a
eval user project target cmdmode assets = Free.iterM go
  where
    go :: Lang.CompilerF (Eval a) -> Eval a
    go = \case
      Lang.Tabstop k ->
        k (project ^. #projectConfig . #projectTabstop)
      Lang.UnwantedDivClasses k ->
        k (target ^. #targetRemoveDivs)
      Lang.Asset file k -> do
        let indir = project ^. #projectTopLevel . #projectDirectory
            outdir = project ^. #projectConfig . #projectOutputDirectory
            ext = Asset.assetExtension (Project.targetFormat target)
        Eval.depends (Input.FromFile file) abort $ \case
          Nothing -> do
            warn
              ( "internal error: failed to make path to asset absolute: "
                  <> toText file
              )
            k file
          Just path
            | HashMap.member (FilePath.takeExtension path) assets -> do
              let output =
                    FilePath.toOutputPath indir outdir path
                      `FilePath.addExt` ext
              shake (Shake.need [output])
              relative <- Eval.relativeToOutput indir outdir output
              k relative
            | otherwise -> do
              shake (Shake.need [path])
              relative <- Eval.relativeToOutput indir outdir path
              k relative
      Lang.ReadInput input token subexp k -> do
        x <- Eval.withInput input abort $ \path content -> do
          whenJust path (shake . Shake.need . one)
          narrowed <-
            case token of
              Nothing ->
                pure content
              Just t ->
                Format.narrow (Format.fromInput input) t content
                  & either (abort . Lang.FormatError input) pure
          eval user project target cmdmode assets (subexp narrowed)
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
            shake $ Shake.command copts (toString approved) []
          k (decodeUtf8 output)
      Lang.Abort e ->
        abort e

    warn :: Text -> Eval ()
    warn = toString >>> Shake.putWarn >>> shake

    abort :: Lang.Error -> Eval a
    abort = throwError

    -- Verify a command fingerprint then call the given continuation.
    verifyCommandWithBypass ::
      -- | Command to verify.
      Text ->
      -- | Continuation to call if command is allowed.
      (Text -> Eval a) ->
      Eval a
    verifyCommandWithBypass command f =
      case cmdmode of
        RequireCommandFingerprints ->
          Eval.verifyCommand (user ^. #userCommandAllowDir) command abort f
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
        let Project.SizeHints {..} = project ^. #projectConfig . #projectSizeHints
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
fileExtensionRule project (inputExt, extOut) f =
  let dir = project ^. #projectConfig . #projectOutputDirectory
      extIn = case inputExt of
        FilePath.FromProjectInput x -> x
        FilePath.FromBuildProduct _ x -> x
      filePattern =
        dir <> "//*"
          <> FilePath.fromExt extIn
          <> FilePath.fromExt extOut
   in filePattern Shake.%> \output -> do
        let indir = project ^. #projectTopLevel . #projectDirectory
            outdir = project ^. #projectConfig . #projectOutputDirectory
            input = FilePath.toInputPathViaInputExt indir outdir inputExt output
        f input output

-- | Generate Shake rules for each asset compiler listed in the 'Asset.AssetMap'.
--
-- @since 0.5.0.0
assetRules ::
  -- | Project configuration.
  Project.Project ->
  -- | The map of asset compilers.
  Asset.AssetMap ->
  -- | Generated Shake rules.
  Shake.Rules ()
assetRules project =
  hmFoldMapWithKey $ \ext compilers ->
    for_ (enumFrom minBound) $ \format ->
      let extIn = FilePath.FromProjectInput ext
          extOut = Asset.assetExtension format
          compiler = curry $ assetEval project (compilers format)
       in fileExtensionRule project (extIn, extOut) compiler
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
  User.User ->
  Project.Project ->
  Project.Target ->
  CommandSafety ->
  Asset.AssetMap ->
  Shake.Rules ()
markdownRule user project target cmdmode assets =
  let extIn = FilePath.FromProjectInput (FilePath.Ext "md")
      extOut = Project.targetFileExtension target
   in fileExtensionRule project (extIn, extOut) action
  where
    action :: FilePath -> FilePath -> Shake.Action ()
    action input output = do
      let compiler = Markdown.compile (Input.FromFile input)

      -- Depend on the project configuration:
      whenJust (project ^. #projectReadFrom) (Shake.need . one)

      ast <-
        eval user project target cmdmode assets compiler
          & runExceptT
          & evaluatingStateT Eval.emptyRuntime
          >>= either (liftIO . throwIO) pure
      liftIO (Utf8.writeFile output (ast & AST.markdownT & Builder.toLazyText))

-- | Convert a target's command into a Shake rule.
--
-- Example:
--
--   Input File: build/content/foo.md.slides
--   Output File: build/content/foo.slides.pdf
--
-- @since 0.5.0.0
targetRule ::
  Project.Project ->
  Project.Target ->
  Shake.Rules ()
targetRule project target =
  let extIn =
        FilePath.FromBuildProduct
          (FilePath.Ext "md")
          (Project.targetFileExtension target)
      extOut = Project.formatExtension (Project.targetFormat target)
   in fileExtensionRule project (extIn, extOut) action
  where
    action :: FilePath -> FilePath -> Shake.Action ()
    action input output = do
      let ops =
            [ Shake.Cwd (project ^. #projectTopLevel . #projectDirectory),
              Shake.Shell,
              Shake.EchoStdout False,
              Shake.EchoStderr False
            ]
          cmd = Project.targetCommand target (input, output)
      Shake.need [input]
      Shake.command_ ops (toString cmd) []

-- | All Shake rules for building an entire project.
--
-- @since 0.5.0.0
rules ::
  User.User ->
  Project.Project ->
  CommandSafety ->
  Asset.AssetMap ->
  Shake.Rules ()
rules user project cmdmode assets = do
  assetRules project assets

  for_ (project ^. #projectConfig . #projectTargets) $ \target -> do
    markdownRule user project target cmdmode assets
    targetRule project target

    for_ (project ^. #projectInputs . #projectInputFiles) $ \file ->
      let indir = project ^. #projectTopLevel . #projectDirectory
          outdir = project ^. #projectConfig . #projectOutputDirectory
          targetE = Project.targetFileExtension target
          format = Project.formatExtension (Project.targetFormat target)
          output =
            FilePath.toOutputPath indir outdir file
              `FilePath.replaceExt` (targetE <> format)
       in Shake.want [output]

-- | Convert a Shake exception into an error message to display to the user.
--
-- @since 0.5.0.0
exceptionToDoc :: Project.Project -> Shake.ShakeException -> P.Doc P.AnsiStyle
exceptionToDoc project e =
  case fromException (Shake.shakeExceptionInner e) of
    Just ee -> Error.render project ee
    Nothing ->
      let toS = show >>> words >>> map P.pretty
       in P.vcat
            [ P.fillSep
                [ "system error while building project file:",
                  P.yellow (P.pretty $ Shake.shakeExceptionTarget e)
                ],
              P.callout (P.red (P.fillSep (toS $ Shake.shakeExceptionInner e)))
            ]

-- | Build a project, returning a set of files that were used during
-- the build.
--
-- @since 0.5.0.0
build ::
  Project.Project ->
  Shake.ShakeOptions ->
  Shake.Rules () ->
  IO (Either (P.Doc P.AnsiStyle) (Set Text))
build project opts rules = (`catch` onerr) $ do
  (after, files) <- Shake.shakeWithDatabase opts rules $ \db -> do
    Shake.shakeOneShotDatabase db
    after <- Shake.shakeRunDatabase db [] <&> snd
    files <- Shake.shakeLiveFilesDatabase db
    pure (after, foldr (Set.insert . toText) mempty files)
  Shake.shakeRunAfter opts after
  pure (Right files)
  where
    onerr :: Shake.ShakeException -> IO (Either (P.Doc P.AnsiStyle) (Set Text))
    onerr = exceptionToDoc project >>> pure . Left

-- | Watch a project for file changes and rebuild it.
--
-- @since 0.5.0.0
watch :: Project.Project -> Shake.ShakeOptions -> Shake.Rules () -> IO ()
watch project opts rules = do
  sem <- newMVar ()
  files <- newMVar mempty

  FSNotify.withManager $ \manager -> do
    _stop <-
      FSNotify.watchTree
        manager
        (project ^. #projectTopLevel . #projectDirectory)
        (FSNotify.eventPath >>> notInBuildDirectory)
        (FSNotify.eventPath >>> notify sem files)
    forever
      ( takeMVar sem
          >> build project opts rules
          >>= \case
            Left e -> P.putError e
            Right s ->
              swapMVar files s
                >> P.putNote "sleeping until a file changes"
      )
  where
    notify :: MVar () -> MVar (Set Text) -> FilePath -> IO ()
    notify sem filesv changed = do
      files <- readMVar filesv
      when (Set.member (toText changed) files) $ do
        let root = project ^. #projectTopLevel . #projectDirectory
        P.putNote
          ( P.fillSep
              [ P.reflow "file changed:",
                P.file (Just root) changed
              ]
          )
        putMVar sem ()

    -- Avoid wake ups by ignoring files in the build directory.
    notInBuildDirectory :: FilePath -> Bool
    notInBuildDirectory =
      let build = project ^. #projectConfig . #projectOutputDirectory
       in not . (build `isPrefixOf`)

-- | Build an Edify project.
--
-- @since 0.5.0.0
main ::
  User.User ->
  Project.Project ->
  Options ->
  IO ()
main user project Options {..} = do
  jobs <-
    if optionsThreads >= 1
      then pure optionsThreads
      else do
        procs <- GHC.getNumProcessors
        pure (max 1 (procs `div` 2))

  go
    (shakeOptions jobs)
    (rules user project optionsCommandSafety Asset.assets)
  where
    shakeOptions :: Int -> Shake.ShakeOptions
    shakeOptions jobs =
      Shake.shakeOptions
        { Shake.shakeFiles = project ^. #projectConfig . #projectOutputDirectory,
          Shake.shakeThreads = jobs
        }

    go :: Shake.ShakeOptions -> Shake.Rules () -> IO ()
    go opts rules
      | optionsWatch = watch project opts rules
      | otherwise = build project opts rules >>= either Exit.withError (const pass)
