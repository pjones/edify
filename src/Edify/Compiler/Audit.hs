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
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Edify.Compiler.Error as Error
import qualified Edify.Compiler.Eval as Eval
import qualified Edify.Compiler.Fingerprint as Fingerprint
import qualified Edify.Compiler.Lang as Lang
import qualified Edify.Compiler.Markdown as Markdown
import qualified Edify.Input as Input
import Edify.JSON
import qualified Edify.Text.Indent as Indent
import qualified Prettyprinter.Render.Terminal as PP
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
type AuditT m a = Eval.Eval (ExceptT (Eval.Runtime, Error.Error) m) a

-- | Evaluate the 'Lang.Compiler' DSL producing an audit report.
--
-- @since 0.5.0.0
eval ::
  MonadIO m =>
  -- | Directory where allow files are stored.
  FilePath ->
  Lang.Compiler a ->
  AuditT m (a, Audit)
eval allowDir = Free.iterM go . fmap (,mempty)
  where
    go :: MonadIO m => Lang.CompilerF (AuditT m (a, Audit)) -> AuditT m (a, Audit)
    go = \case
      Lang.Tabstop k ->
        k Indent.defaultTabstop
      Lang.UnwantedDivClasses k ->
        k mempty -- Keep all divs.
      Lang.Asset file k -> do
        abs <- Eval.depends (Input.FromFile file) abort (maybe (pure file) pure)
        k abs <&> second (embed (AuditAsset abs) <>)
      Lang.ReadInput input _token subexp k -> do
        -- N.B.: Narrowing token is ignored so we see entire files.
        (x, a) <- Eval.withInput input abort $ \path content ->
          eval allowDir (subexp content)
            <&> ( case path of
                    Nothing -> id
                    Just file -> second (embed . AuditFile file)
                )
        k x <&> second (a <>)
      Lang.Exec (command, _input) k -> do
        (path, status) <- Eval.commandStatus allowDir command abort (curry pure)
        k command <&> second (embed (AuditCommand path command status) <>)
      Lang.Abort e ->
        abort e

    abort :: Monad m => Error.Error -> AuditT m a
    abort e = do
      runtime <- get
      throwError (runtime, e)

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
  m (Either (Eval.Runtime, Error.Error) Audit)
audit allowDir files =
  runExceptT $
    foldrM
      (\x y -> (y <>) <$> go (Markdown.compile $ Input.FromFile x))
      mempty
      files
  where
    go :: Lang.Compiler a -> ExceptT (Eval.Runtime, Error.Error) m Audit
    go =
      eval allowDir
        >>> evaluatingStateT Eval.emptyRuntime
        >>> runExceptT
        >>> fmap (second snd)
        >>> ExceptT

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
  Maybe (PP.Doc PP.AnsiStyle)
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

    render :: (FilePath, Text) -> PP.Doc PP.AnsiStyle
    render (path, cmd) =
      mconcat
        [ PP.sep
            [ ppStatus Fingerprint.Mismatch <> PP.colon,
              PP.dquotes (PP.pretty cmd)
            ],
          PP.nest
            2
            ( PP.line
                <> PP.sep ["From file:", ppFilePath inputDir path]
            ),
          PP.line
        ]

-- | Product a full report using the given audit trail.
--
-- @since 0.5.0.0
fullReport :: FilePath -> Audit -> PP.Doc PP.AnsiStyle
fullReport inputDir = cata go >>> (<> PP.line)
  where
    go :: AuditF (PP.Doc PP.AnsiStyle) -> PP.Doc PP.AnsiStyle
    go = \case
      AuditEnd -> mempty
      AuditItems docs -> fold docs
      AuditAsset path ->
        PP.line <> PP.sep ["Asset" <> PP.colon, ppFilePath inputDir path]
      AuditFile path doc ->
        let entry = PP.sep ["Read" <> PP.colon, ppFilePath inputDir path]
         in PP.line <> entry <> PP.nest 2 doc
      AuditCommand _path command status ->
        let entry =
              PP.sep
                [ "Exec" <> PP.colon,
                  ppStatus status,
                  PP.pretty command
                ]
         in PP.line <> entry

-- | Pretty print a file path.
--
-- @since 0.5.0.0
ppFilePath :: FilePath -> FilePath -> PP.Doc ann
ppFilePath dir = FilePath.makeRelative dir >>> PP.pretty

-- | Pretty print a fingerprint status.
--
-- @since 0.5.0.0
ppStatus :: Fingerprint.Status -> PP.Doc PP.AnsiStyle
ppStatus = \case
  Fingerprint.Mismatch ->
    let doc = PP.brackets "BLOCKED"
     in PP.annotate (PP.color PP.Red) doc
  Fingerprint.Verified ->
    let doc = PP.brackets "ALLOWED"
     in PP.annotate (PP.color PP.Green) doc

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
main mode allowDir inputDir inputs =
  audit allowDir inputs >>= \case
    Left (r, e) ->
      -- FIXME: Need proper error reporting.
      print r >> print e
    Right info ->
      case mode of
        JsonAuditMode -> putLBS (Aeson.encode info)
        FullAuditMode ->
          printReport (fullReport inputDir info)
        BlockedCommandAuditMode ->
          case blockedReport inputDir info of
            Nothing -> pass
            Just r -> do
              printReport r
              exitFailure
  where
    printReport :: PP.Doc PP.AnsiStyle -> IO ()
    printReport = PP.putDoc
