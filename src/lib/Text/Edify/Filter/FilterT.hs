{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  , pushdir
  , popdir
  , realpath
  , runFilterT
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad (when)
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (MonadState, gets, modify)
import Control.Monad.State.Lazy (StateT, evalStateT)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import System.Directory (canonicalizePath, setCurrentDirectory)

--------------------------------------------------------------------------------
-- | Internal state.
data State = State
  { statePWD :: NonEmpty FilePath
  }

--------------------------------------------------------------------------------
-- | Filter errors.
data Error = Error String
           | EmptyPopdir FilePath

instance Show Error where
  show (Error s) = s
  show (EmptyPopdir f) = "popdir called on single element stack " ++ f

--------------------------------------------------------------------------------
-- | Internal monad transformer for filters.
newtype FilterT m a = FilterT
  { unF :: StateT State (ExceptT Error m) a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadError Error
           , MonadState State
           )

--------------------------------------------------------------------------------
-- | Fetch the current directory name (the name of the directory where
-- the current input file lives).
pwd :: (Monad m) => FilterT m FilePath
pwd = gets (NonEmpty.head . statePWD)

--------------------------------------------------------------------------------
-- | Add a directory to directory stack.  This will also change the
-- working directory for the current process so other filters work as
-- expected.
pushdir :: (MonadIO m) => FilePath -> FilterT m ()
pushdir dir = do
  dir' <- realpath dir
  (x :| xs) <- gets statePWD
  modify (\s -> s {statePWD = dir' :| (x:xs)})
  liftIO (setCurrentDirectory dir')

--------------------------------------------------------------------------------
-- | Remove the current directory from the stack and restore the
-- previous working directory for the current process.
popdir :: (MonadIO m) => FilterT m ()
popdir = do
  (x :| xs) <- gets statePWD
  when (null xs) $ throwError (EmptyPopdir x)
  modify (\s -> s {statePWD = head xs :| tail xs})
  liftIO (setCurrentDirectory $ head xs)

--------------------------------------------------------------------------------
-- | Transform the given 'FilePath' so that it's an absolute path with
-- respect to the current filter directory (set with 'pushdir').
realpath :: (MonadIO m) => FilePath -> FilterT m FilePath
realpath = liftIO . canonicalizePath

--------------------------------------------------------------------------------
-- | Run a 'FilterT' operation.
runFilterT :: (MonadIO m)
           => FilePath          -- ^ The directory of the input file.
           -> FilterT m a       -- ^ The filter operation
           -> m (Either Error a)

runFilterT dir f = do
    dir' <- liftIO (canonicalizePath dir)
    runExceptT (evalStateT (unF f) (initS dir'))

  where
    initS :: FilePath -> State
    initS d = State { statePWD = pure d
                    }
