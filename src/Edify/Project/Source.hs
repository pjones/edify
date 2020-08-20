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
-- Process a source file, producing a final input for the format
-- parser.
module Edify.Project.Source
  ( Source (..),
    Error (..),
    Context,
    toSource,
    fromSource,
  )
where

import qualified Byline as B
import Control.Lens ((+~), (.~))
import Control.Monad.Except
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Graph.Inductive.PatriciaTree as Graph
import qualified Data.Graph.Inductive.Query.DFS as Graph
import qualified Data.Text.Lazy.Builder as LTB
import qualified Edify.Format as Format
import Edify.Input (Input)
import qualified Edify.Input as Input
import Edify.JSON
import qualified Edify.Text.Inclusion as Inclusion
import Edify.Text.Narrow (Token (..))
import System.FilePath ((</>))

-- | The final source document file name and content.
--
-- @since 0.5.0.0
data Source = Source FilePath Text
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via GenericJSON Source

-- | Errors that may occur while processing the source file.
--
-- @since 0.5.0.0
data Error
  = InputError FilePath Input.Error
  | InclusionError FilePath Inclusion.Error
  | InclusionCycleError FilePath [FilePath] (Graph.Gr FilePath ())
  | FileNodeMissingError FilePath (Graph.Gr FilePath ())
  | FormatError FilePath Format.Error
  deriving (Generic, Show)

instance B.ToStylizedText Error where
  toStylizedText = \case
    InputError file e ->
      filePrefix file <> B.toStylizedText e
    InclusionError file e ->
      filePrefix file <> B.toStylizedText e
    InclusionCycleError file files graph ->
      filePrefix file
        <> "an inclusion cycle was found in the in the following files:\n\n"
        <> foldMap (\f -> B.text "  * " <> B.text (toText f) <> "\n") files
        <> "\nThe complete inclusion graph follows:\n"
        <> B.text (toText $ Graph.prettify graph)
    FileNodeMissingError file graph ->
      B.text "internal inconsistency error: "
        <> B.text "I should have been able to find "
        <> (B.text (toText file) <> B.fg B.green)
        <> B.text " in the inclusion graph but it's not there!\n"
        <> "\nThe complete inclusion graph follows:\n"
        <> B.text (toText $ Graph.prettify graph)
    FormatError file e ->
      filePrefix file <> B.toStylizedText e
    where
      filePrefix :: FilePath -> B.Stylized Text
      filePrefix file =
        B.text "while processing file "
          <> (B.text (toText file) <> B.fg B.green)
          <> ": "

-- | Context needed while processing a source file.
--
-- @since 0.5.0.0
data Context = Context
  { inclusionGraph :: Graph.Gr FilePath (),
    nextGraphNodeID :: Int
  }
  deriving (Generic)

-- | Internal type for detecting cycles.
data Cycles
  = NoCycles
  | Cycles [Graph.Node]

-- | Detect if the given graph has any cycles.
checkForCycles :: Graph.Gr FilePath () -> Cycles
checkForCycles g = foldr (check []) NoCycles (Graph.nodes g)
  where
    check :: [Graph.Node] -> Graph.Node -> Cycles -> Cycles
    check _ _ (Cycles ns) = Cycles ns
    check stack n NoCycles =
      let rs = Graph.reachable n g
          stack' = stack ++ [n]
       in case rs of
            [] -> NoCycles
            [_] -> NoCycles
            _ : ns ->
              if any (`elem` stack') ns
                then Cycles (stack' ++ filter (`elem` stack') ns)
                else foldr (check stack') NoCycles ns

-- | Ensure that including the given file does not cause a cycle.
--
-- @since 0.5.0.0
guardInclusionCycle ::
  MonadError Error m =>
  MonadState Context m =>
  -- | The file being processed.
  FilePath ->
  -- | The file to be included.
  FilePath ->
  -- | Error or unit.
  m ()
guardInclusionCycle src other = do
  Context {..} <- get
  srcid <-
    lookupNodeID src inclusionGraph
      & maybe (throwError (FileNodeMissingError src inclusionGraph)) pure
  let dstid = fromMaybe nextGraphNodeID (lookupNodeID other inclusionGraph)
      node = (dstid, other)
      edge = (srcid, dstid, ())
      graph = inclusionGraph & insNode node & insEdge edge
  case checkForCycles graph of
    NoCycles ->
      modify'
        ( #inclusionGraph .~ graph
            >>> #nextGraphNodeID +~ 1
        )
    Cycles ns ->
      throwError $
        InclusionCycleError
          src
          (mapMaybe (Graph.lab graph) ns) -- File Names
          graph
  where
    lookupNodeID :: FilePath -> Graph.Gr FilePath () -> Maybe Int
    lookupNodeID file =
      Graph.labNodes
        >>> filter (snd >>> (== file))
        >>> listToMaybe
        >>> fmap fst
    -- Only insert unique nodes otherwise the cycle detection fails.
    insNode :: Graph.LNode FilePath -> Graph.Gr FilePath () -> Graph.Gr FilePath ()
    insNode n g
      | Graph.gelem (fst n) g = g
      | otherwise = Graph.insNode n g
    -- Only insert unique edges otherwise the cycle detection fails.
    insEdge :: Graph.LEdge () -> Graph.Gr FilePath () -> Graph.Gr FilePath ()
    insEdge e g
      | Graph.hasLEdge g e = g
      | otherwise = Graph.insEdge e g

-- | Resolve an inclusion chuck.  This might require loading and
-- narrowing another file.
--
-- @since 0.5.0.0
resolveChunk ::
  MonadIO m =>
  MonadState Context m =>
  MonadError Error m =>
  FilePath ->
  Inclusion.Chunk ->
  m [Source]
resolveChunk file = \case
  Inclusion.Chunk t -> pure [Source file t]
  Inclusion.Include other token -> do
    let absolute = Input.makeFilePathAbsoluteTo file other
        input = Input.filePathToInput (Just absolute)
    guardInclusionCycle file absolute
    text <-
      Input.readInput input
        >>= either (InputError file >>> throwError) pure
    toSourceChunks (Token <$> token) absolute text

-- | Convert the given input into a 'Source' list.
--
-- @since 0.5.0.0
toSourceChunks ::
  MonadIO m =>
  MonadError Error m =>
  MonadState Context m =>
  -- | An optional token for text narrowing.
  Maybe Token ->
  -- | The name of the file from which the input came.
  FilePath ->
  -- | The input.
  LText ->
  -- | The result list.
  m [Source]
toSourceChunks token file input = do
  content <- liftEither (narrow token input)
  chunks <-
    liftEither
      ( Inclusion.toChunks (toStrict content)
          & first (InclusionError file)
      )
  traverse (resolveChunk file) chunks <&> concat
  where
    narrow :: Maybe Token -> LText -> Either Error LText
    narrow = \case
      Nothing ->
        Right
      Just t ->
        let format = Format.fromFileExtension file
         in Format.narrow format t >>> first (FormatError file)

-- | Convert the given input into a 'Source' list.
--
-- @since 0.5.0.0
toSource :: MonadIO m => Input -> m (Either Error ([Source], Context))
toSource input = do
  file <- Input.filePathFromInput input <&> either (</> "<unknown>") id
  Input.readInput input >>= \case
    Left e -> pure $ Left (InputError file e)
    Right text ->
      toSourceChunks Nothing file text
        & usingStateT (Context (initialGraph file) 1)
        & runExceptT
  where
    initialGraph :: FilePath -> Graph.Gr FilePath ()
    initialGraph file = Graph.empty & Graph.insNode (0, file)

-- | Convert a 'Source' list back into 'Text'.
--
-- @since 0.5.0.0
fromSource :: [Source] -> ([FilePath], LTB.Builder)
fromSource =
  foldl'
    ( \(files, builder) (Source f t) ->
        (f : files, builder <> LTB.fromText t)
    )
    ([], mempty)
