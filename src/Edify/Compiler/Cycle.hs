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
-- Detect dependency cycles.
module Edify.Compiler.Cycle
  ( Deps,
    emptyDeps,
    render,
    Cycles (..),
    depends,
  )
where

import Control.Lens ((.~))
import Data.Generics.Labels ()
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Graph.Inductive.PatriciaTree as Graph
import qualified Data.Graph.Inductive.Query.DFS as Graph
import Prelude hiding (empty)

-- | Collection of dependencies.
--
-- @since 0.5.0.0
data Deps a = Deps
  { -- | Dependency graph.
    depsGraph :: !(Graph.Gr a ()),
    -- | Counter for generating graph node IDs.
    depsCounter :: !Int
  }
  deriving (Generic, Show)

-- | Create a new, empty dependency graph.
--
-- @since 0.5.0.0
emptyDeps :: Deps a
emptyDeps = Deps Graph.empty 0

-- | Render the graph for debugging.
--
-- @since 0.5.0.0
render :: Show a => Deps a -> String
render Deps {..} = Graph.prettify depsGraph

-- | The result of checking for dependency cycles.
--
-- @since 0.5.0.0
data Cycles a
  = -- | No dependency cycles were found.
    NoCycles
  | -- | A dependency cycle was found among the given items.
    Cycles ![a]
  deriving stock (Generic, Show)

-- | Detect if the given graph has any cycles.
checkForCycles :: forall a. Graph.Gr a () -> Cycles a
checkForCycles g = foldr (check []) NoCycles (Graph.nodes g)
  where
    -- Check to see if the given node forms a cycle.  A cycle is when
    -- a series of edges connect the node back to itself.
    --
    -- The FGL library does not have a cycle detection function so we
    -- have to walk the graph recursively looking for cycles.  This
    -- function will short circuit as soon as it finds a cycle.
    check :: [Graph.Node] -> Graph.Node -> Cycles a -> Cycles a
    check _stack _n c@Cycles {} = c
    check stack n NoCycles
      | Graph.hasLEdge g (n, n, ()) = found [n]
      | otherwise =
        let stack' = stack ++ [n]
         in case drop 1 (Graph.reachable n g) of
              [] ->
                NoCycles
              ns
                | any (`elem` stack') ns ->
                  found (stack' ++ filter (`elem` stack') ns)
                | otherwise ->
                  foldr (check stack') NoCycles ns

    found :: [Graph.Node] -> Cycles a
    found = Cycles . mapMaybe (Graph.lab g)

-- | Record a dependency and check for any cycles.
--
-- @since 0.5.0.0
depends ::
  forall a.
  Eq a =>
  -- | The source entry.
  a ->
  -- | The destination entry (dependency).
  a ->
  -- | Existing dependency graph.
  Deps a ->
  -- | The results of cycle detection along with a new dependency
  -- graph.
  (Cycles a, Deps a)
depends src dest = runState $ do
  srcid <- makeNodeID src
  destid <- makeNodeID dest

  graph <-
    gets depsGraph
      <&> insEdge (srcid, destid, ())

  modify (#depsGraph .~ graph)
  pure (checkForCycles graph)
  where
    -- Either find the existing node ID for the given label or create
    -- a new node ID and insert it into the graph.
    makeNodeID :: a -> State (Deps a) Int
    makeNodeID label = do
      Deps {..} <- get

      Graph.labNodes depsGraph
        & filter (snd >>> (== label))
        & listToMaybe
        & fmap fst
        & \case
          Nothing ->
            let node = (depsCounter, label)
                graph = depsGraph & insNode node
             in put (Deps graph (depsCounter + 1)) $> depsCounter
          Just node ->
            pure node

    -- Only insert unique nodes otherwise the cycle detection fails.
    insNode :: Graph.LNode a -> Graph.Gr a () -> Graph.Gr a ()
    insNode n g
      | Graph.gelem (fst n) g = g
      | otherwise = Graph.insNode n g

    -- Only insert unique edges otherwise the cycle detection fails.
    insEdge :: Graph.LEdge () -> Graph.Gr a () -> Graph.Gr a ()
    insEdge e g
      | Graph.hasLEdge g e = g
      | otherwise = Graph.insEdge e g
