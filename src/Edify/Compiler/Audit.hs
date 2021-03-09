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
module Edify.Compiler.Audit
  ( Mode (..),
    AuditF (..),
    Audit,
    eval,
    audit,
    main,
  )
where

import Control.Monad.Except (throwError)
import qualified Control.Monad.Free.Church as Free
import qualified Data.Aeson as Aeson
import Data.Foldable (foldrM)
import Data.Functor.Foldable (Fix (..), cata, embed)
import Data.Generics.Labels ()
import qualified Data.List.NonEmpty as NonEmpty
import qualified Edify.Compiler.Error as Error
import qualified Edify.Compiler.Eval as Eval
import qualified Edify.Compiler.Lang as Lang
import qualified Edify.Compiler.Markdown as Markdown
import qualified Edify.Compiler.Stack as Stack
import qualified Edify.Project as Project
import qualified Edify.System.Exit as Exit
import qualified Edify.System.Input as Input
import qualified Edify.Text.Fingerprint as Fingerprint
import qualified Edify.Text.Indent as Indent
import Edify.Text.JSON
import qualified Edify.Text.Pretty as P
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

-- | What type of output to produce.
--
-- @since 0.5.0.0
data Mode
  = BlockedCommandAuditMode
  | FullAuditMode
  | JsonAuditMode

-- | An entry in the audit log.  Recursion is extracted out in the
-- type variable thanks to @recursion-schemes@.
--
-- @since 0.5.0.0
data AuditF r
  = -- | Base case.
    AuditEnd
  | -- | Used to connect two audits together.
    AuditItems (NonEmpty r)
  | -- | An asset was referenced in the Markdown.
    AuditAsset
      { auditAssetPath :: FilePath
      }
  | -- | File opened for reading.
    AuditFile
      { auditFilePath :: FilePath,
        auditFileNext :: r
      }
  | -- | A request to run a command.
    AuditCommand
      { auditCommandFrom :: FilePath,
        auditCommandText :: Text,
        auditCommandStatus :: Fingerprint.Status
      }
  deriving stock (Generic, Functor, Foldable, Traversable)
  deriving (ToJSON, FromJSON) via GenericJSON (AuditF r)

-- | Fully recursive variant of 'AuditF'.
--
-- @since 0.5.0.0
type Audit = Fix AuditF

deriving via (RecursiveJSON Audit) instance ToJSON Audit

deriving via (RecursiveJSON Audit) instance FromJSON Audit

instance Semigroup Audit where
  (<>) (Fix (AuditItems xs)) (Fix (AuditItems ys)) = embed $ AuditItems (xs <> ys)
  (<>) x (Fix (AuditItems xs)) = embed $ AuditItems (NonEmpty.cons x xs)
  (<>) (Fix (AuditItems xs)) x = embed $ AuditItems (xs <> one x)
  (<>) x y = embed $ AuditItems (x :| [y])

instance Monoid Audit where
  mempty = embed AuditEnd

-- | Transformer stack needed when auditing Markdown.
--
-- @since 0.5.0.0
type AuditT m a = StateT Eval.Runtime (ExceptT Error.Error m) a

-- | Evaluate the 'Lang.Compiler' DSL producing an audit report.
--
-- @since 0.5.0.0
eval ::
  MonadIO m =>
  -- | Directory where allow files are stored.
  FilePath ->
  Lang.Compiler a ->
  AuditT m (a, Audit)
eval allowDir compiler = do
  stack <- gets Eval.stack
  let initialFile = Stack.top stack & Input.toFilePath & fromMaybe "<stdin>"
  eval' compiler <&> second (embed . AuditFile initialFile)
  where
    eval' :: MonadIO m => Lang.Compiler a -> AuditT m (a, Audit)
    eval' = Free.iterM alg . fmap (,mempty)

    alg :: MonadIO m => Lang.CompilerF (AuditT m (a, Audit)) -> AuditT m (a, Audit)
    alg = \case
      Lang.Tabstop k ->
        k Indent.defaultTabstop
      Lang.UnwantedDivClasses k ->
        k mempty -- Keep all divs.
      Lang.Asset file k -> do
        abs <- Eval.depends file
        k abs <&> second (embed (AuditAsset abs) <>)
      Lang.WithFileContents file _token subexp k -> do
        -- N.B.: Narrowing token is ignored so we see entire files.
        (x, a) <- Eval.withFileContents file Nothing $ \path content ->
          eval' (subexp content)
            <&> second (embed . AuditFile path)
        k x <&> second (a <>)
      Lang.Exec (command, _input) k -> do
        (path, status) <- Eval.commandStatus allowDir command (curry pure)
        k command <&> second (embed (AuditCommand path command status) <>)
      Lang.Abort f -> do
        top <- gets Eval.stack <&> Stack.top
        abort (f top)

    abort :: Monad m => Error.Error -> AuditT m a
    abort = throwError

-- | Wrapper around 'eval' that also discharges the transformer stack.
--
-- @since 0.5.0.0
audit ::
  forall m.
  MonadIO m =>
  -- | Directory where allow files are stored.
  FilePath ->
  -- | Files to audit.
  NonEmpty FilePath ->
  -- | The audit report.
  m (Either Error.Error Audit)
audit allowDir files = runExceptT $ do
  foldrM
    ( \file y ->
        fmap (y <>) $
          if isConfigFile file
            then projectAudit file
            else markdownAudit file
    )
    mempty
    files
  where
    markdownAudit :: FilePath -> ExceptT Error.Error m Audit
    markdownAudit file = do
      let input = Input.FromFile file

      content <-
        Input.readInput input
          >>= either (throwError . (`Error.InputError` input)) pure

      eval allowDir (Markdown.compile content)
        & evaluatingStateT (Eval.emptyRuntime input)
        & runExceptT
        & fmap (second snd)
        & ExceptT

    projectAudit :: FilePath -> ExceptT Error.Error m Audit
    projectAudit file = do
      let input = Input.FromFile file
      evaluatingStateT (Eval.emptyRuntime input) $ do
        (project :: Project.ProjectConfig Parsed) <-
          Input.decodeFromFile Input.ReadWithoutFingerprint file
            >>= either (throwError . (`Error.InputError` input)) pure
        traverse
          (\cmd -> Eval.commandStatus allowDir cmd (const $ pure . (cmd,)))
          (Project.projectCommands project)
          <&> foldMap (\(cmd, status) -> embed (AuditCommand file cmd status))

    isConfigFile :: FilePath -> Bool
    isConfigFile =
      FilePath.takeFileName
        >>> (`elem` Project.projectConfigFiles)

-- | Product a report that only includes commands that are blocked
-- from running.
--
-- @since 0.5.0.0
blockedReport ::
  -- | Project input directory.
  FilePath ->
  -- | The audit value to use.
  Audit ->
  -- | Nothing when no commands are blocked.
  Maybe (P.Doc P.AnsiStyle)
blockedReport inputDir =
  blocked >>> \case
    [] -> Nothing
    xs ->
      sortNub xs
        & foldMap render
        & Just
  where
    blocked :: Audit -> [(FilePath, Text)]
    blocked = cata $ \case
      AuditEnd -> mempty
      AuditItems items -> fold items
      AuditAsset _ -> mempty
      AuditFile _ children -> children
      AuditCommand path cmd status ->
        case status of
          Fingerprint.Mismatch -> [(path, cmd)]
          Fingerprint.Verified -> []

    render :: (FilePath, Text) -> P.Doc P.AnsiStyle
    render (path, cmd) =
      mconcat
        [ P.nest 2 $
            P.vcat
              [ ppStatus Fingerprint.Mismatch <> P.colon,
                P.fillSep ["command" <> P.colon, P.command cmd],
                P.fillSep
                  [ "from file" <> P.colon,
                    P.file (Just inputDir) path
                  ]
              ],
          P.hardline
        ]

-- | Product a full report using the given audit trail.
--
-- @since 0.5.0.0
fullReport :: FilePath -> Audit -> P.Doc P.AnsiStyle
fullReport inputDir = cata go >>> maybe mempty (<> P.hardline)
  where
    go :: AuditF (Maybe (P.Doc P.AnsiStyle)) -> Maybe (P.Doc P.AnsiStyle)
    go = \case
      AuditEnd -> Nothing
      AuditItems docs ->
        case catMaybes $ toList docs of
          [] -> Nothing
          ds -> Just (P.vcat ds)
      AuditAsset path ->
        Just (P.sep ["Asset" <> P.colon, P.file (Just inputDir) path])
      AuditFile path doc ->
        let entry = P.sep ["Read" <> P.colon, P.file (Just inputDir) path]
         in case doc of
              Nothing -> Just entry
              Just d -> Just (entry <> P.nest 2 (P.hardline <> d))
      AuditCommand _path command status ->
        Just $
          P.sep
            [ "Exec" <> P.colon,
              ppStatus status,
              P.pretty command
            ]

-- | Pretty print a fingerprint status.
--
-- @since 0.5.0.0
ppStatus :: Fingerprint.Status -> P.Doc P.AnsiStyle
ppStatus = \case
  Fingerprint.Mismatch ->
    let doc = P.brackets "BLOCKED"
     in P.annotate (P.color P.Red) doc
  Fingerprint.Verified ->
    let doc = P.brackets "ALLOWED"
     in P.annotate (P.color P.Green) doc

-- | Run a report with the given options.
--
-- @since 0.5.0.0
main ::
  -- | Output mode.
  Mode ->
  -- | Directory where allow files are stored.
  FilePath ->
  -- | Project input directory.
  FilePath ->
  -- | List of files to audit.
  NonEmpty FilePath ->
  -- | Audit action.
  IO ()
main mode allowDir inputDir inputs = do
  cwd <- liftIO Directory.getCurrentDirectory
  audit allowDir inputs >>= \case
    Left e ->
      Exit.withError (Error.render' cwd inputDir inputs e)
    Right info ->
      case mode of
        JsonAuditMode -> putLBS (Aeson.encode info)
        FullAuditMode ->
          P.putDoc (fullReport inputDir info)
        BlockedCommandAuditMode ->
          let post =
                P.vcat
                  [ P.reflow "To allow these commands run:",
                    P.callout
                      ( P.edify
                          "allow"
                          (map (P.file (Just cwd)) $ toList inputs)
                      )
                  ]
           in case blockedReport inputDir info of
                Nothing ->
                  exitSuccess
                Just doc -> do
                  P.putDoc (doc <> P.hardline <> post <> P.hardline)
                  exitFailure
