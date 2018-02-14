{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
module Text.Edify.Filter.FilterT
  ( FilterT
  , Error(..)
  , pwd
  , pushfile
  , popfile
  , realpath
  , processPandoc
  , processFile
  , runFilterT
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad (foldM, when)
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.State.Class (MonadState, get, gets, put, modify)
import Control.Monad.State.Lazy (StateT, evalStateT)
import qualified Data.Graph.Analysis.Algorithms.Common as Graph
import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import System.FilePath ((</>), takeDirectory)
import Text.Pandoc.Definition (Pandoc)

import System.Directory ( canonicalizePath
                        , getCurrentDirectory
                        , setCurrentDirectory
                        )

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Edify.Util.Markdown (parseMarkdown)

--------------------------------------------------------------------------------
-- | Internal state.
data State = State
  { stateInputFile :: NonEmpty (Int, FilePath)
    -- ^ A stack of file names currently being processed.

  , stateInclusions :: Gr FilePath ()
    -- ^ A graph of files that have been included into the current
    -- document.  This is used to detect inclusion cycles.

  , stateNodeID :: Int
    -- ^ The next node ID to include in the graph.
  }

--------------------------------------------------------------------------------
type Filters m = [Pandoc -> FilterT m Pandoc]

--------------------------------------------------------------------------------
-- | Internal reader environment.
data Env m = Env
  { envFilters :: Filters m
  }

--------------------------------------------------------------------------------
-- | Filter errors.
data Error = Error String
           | EmptyPopfile FilePath
           | BadMarkdownFile FilePath String
           | CycleFound [FilePath]

instance Show Error where
  show (Error s) = s
  show (EmptyPopfile f) = "popfile called on single element stack " ++ f
  show (BadMarkdownFile f s ) = "failed to parse markdown file " ++ f ++ " :" ++ s
  show (CycleFound fs) = "inclusion cycle: " ++ show fs

--------------------------------------------------------------------------------
-- | Internal monad transformer for filters.
newtype FilterT m a = FilterT
  { unF :: ReaderT (Env m) (StateT State (ExceptT Error m)) a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadError Error
           , MonadReader (Env m)
           , MonadState State
           )

--------------------------------------------------------------------------------
-- | Fetch the current directory name (the name of the directory where
-- the current input file lives).
pwd :: (Monad m) => FilterT m FilePath
pwd = gets (takeDirectory . snd . NonEmpty.head . stateInputFile)

--------------------------------------------------------------------------------
-- | Add a file to the input file stack.  This will also change the
-- working directory for the current process so other filters work as
-- expected.
pushfile :: (MonadIO m) => FilePath -> FilterT m ()
pushfile file = do
  state <- get
  file' <- realpath file

  let (x :| xs) = stateInputFile state
      node      = (nodeID file' state, file')
      edge      = (fst x, fst node, ())
      state'    = State { stateInputFile = node :| (x:xs)
                        , stateNodeID = stateNodeID state + 1
                        , stateInclusions = Graph.insEdge edge $
                                              Graph.insNode node $
                                                stateInclusions state
                        }


  case Graph.cyclesIn (stateInclusions state') of
    [] -> do
      put state'
      liftIO (setCurrentDirectory $ takeDirectory file')

    c:_ -> do
      let names = map snd c
      throwError (CycleFound names)

  where
    nodeID :: FilePath -> State -> Int
    nodeID path state =
      let nodes = Graph.labNodes (stateInclusions state) in
      case filter (\(_,p) -> p == path) nodes of
        []      -> stateNodeID state -- Use the next available ID.
        (i,_):_ -> i                 -- Use the ID that's already in the graph.

--------------------------------------------------------------------------------
-- | Remove the current input file from the stack and restore the
-- previous working directory for the current process.
popfile :: (MonadIO m) => FilterT m ()
popfile = do
  (x :| xs) <- gets stateInputFile
  when (null xs) $ throwError (EmptyPopfile (snd x))
  modify (\s -> s {stateInputFile = head xs :| tail xs})
  liftIO (setCurrentDirectory . takeDirectory . snd $ head xs)

--------------------------------------------------------------------------------
-- | Transform the given 'FilePath' so that it's an absolute path with
-- respect to the current filter directory (set with 'pushfile').
realpath :: (MonadIO m) => FilePath -> FilterT m FilePath
realpath = liftIO . canonicalizePath

--------------------------------------------------------------------------------
-- | Filter the given Pandoc tree.
processPandoc :: (MonadIO m) => Pandoc -> FilterT m Pandoc
processPandoc doc = do
  filters <- asks envFilters
  foldM (flip ($)) doc filters

--------------------------------------------------------------------------------
-- | Load and filter the given Markdown file.
processFile :: (MonadIO m) => FilePath -> FilterT m Pandoc
processFile file = do
  cleanFile <- realpath file
  markdown <- parseMarkdown cleanFile

  doc <- case markdown of
           Left e  -> throwError (BadMarkdownFile file e)
           Right d -> return d

  pushfile cleanFile
  updated <- processPandoc doc
  popfile

  return updated

--------------------------------------------------------------------------------
-- | Run a 'FilterT' operation.
runFilterT :: forall m a. (MonadIO m)
           => Maybe FilePath
           -- ^ The name of the input file.  If processing STDIN
           -- then set this to 'Nothing'.

           -> Filters m
           -- ^ List of filters to apply to processed files.

           -> FilterT m a
           -- ^ The filter operation to run.

           -> m (Either Error a)
           -- ^ Results or error message.

runFilterT Nothing fs f = do
  cwd <- (</> "stdin") <$> liftIO getCurrentDirectory
  runFilterT (Just cwd) fs f

runFilterT (Just file) fs f = do
    cleanFile <- liftIO (canonicalizePath file)

    runExceptT $
      flip evalStateT (initS cleanFile) $
        runReaderT (unF f) initE

  where
    initE :: Env m
    initE =
      Env { envFilters = fs
          }

    initS :: FilePath -> State
    initS path = let node = (1, path) in
      State { stateInputFile = pure node
            , stateInclusions = Graph.insNode node Graph.empty
            , stateNodeID = 2
            }
