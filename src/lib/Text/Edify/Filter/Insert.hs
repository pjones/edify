{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Extra Pandoc features for dealing with source code and for
-- inserting other Pandoc input files.
module Text.Edify.Filter.Insert
  ( insertFile
  , insertParsedFile
  ) where

--------------------------------------------------------------------------------
-- Library imports.
import Control.Applicative
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import Data.Traversable (traverse)
import System.Directory (doesFileExist)
import Text.Pandoc.Definition

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Filter.FilterT
import Text.Edify.Util.Inclusion
import Text.Edify.Util.Indent
import Text.Edify.Util.Narrow

--------------------------------------------------------------------------------
-- | Copies the contents of a file into the body of a code block.  The
-- name of the file is given in the attributes of the code block like
-- so:
--
-- > ~~~~ {insert="README"}
-- > this will be replaced by contents of README
-- > ~~~~
--
-- The file can be narrowed to the text between delimiters.  The
-- delimiters are @<<: token@ and @:>>@.
--
-- > ~~~~ {insert="README" token="foo"}
-- > ~~~~
--
-- For backwards compatibility you can also use the @include@
-- attribute instead of @insert@.
--
-- Taken from <http://johnmacfarlane.net/pandoc/scripting.html#include-files>.
insertFile :: (MonadIO m) => Block -> FilterT m Block
insertFile cb@(CodeBlock (blkid, classes, alist) _) =
  case lookup "insert" alist <|> lookup "include" alist of
    Just f  -> CodeBlock (blkid, classes, map update alist) <$> newtxt f
    Nothing -> return cb
  -- FIXME: the newly created "inserted" attribute should be relative
  -- to the output file.
  where newtxt f = readCodeFile f (lookup "token" alist)
        update (k,v) = if k == "insert" || k == "include"
                       then ("inserted", v)
                       else (k, v)
insertFile x = return x

--------------------------------------------------------------------------------
-- | Read a file with code in it, possibly narrowing to a token.
readCodeFile :: (MonadIO m) => FilePath -> Maybe String -> FilterT m String
readCodeFile path Nothing = do
  verbose ("reading source code file: " ++ path)
  cleanPath <- realpath path
  addDependency cleanPath
  exist <- liftIO (doesFileExist cleanPath)
  unless exist (throwError $ MissingFile cleanPath)
  liftIO (readFile cleanPath)

readCodeFile path (Just token) = do
  contents <- T.pack <$> readCodeFile path Nothing

  case narrow (Token (T.pack token))  contents of
    Right txt -> (return . T.unpack . removeIndent) txt
    Left err  -> throwError (Error $ "can't find token '" ++ token ++
                             "' in " ++ path ++ ": " ++ err)

--------------------------------------------------------------------------------
-- | Replaces file insertion markers with the files they reference.
--
-- Example:
--
-- > # Some Heading
-- >
-- > <<(foo.md)
--
-- The line referencing @foo.md@ will be replaced with its contents.
insertParsedFile :: (MonadIO m) => [Block] -> FilterT m [Block]
insertParsedFile = fmap concat . mapM blk

  where
    blk :: (MonadIO m) => Block -> FilterT m [Block]
    blk b@(Plain xs) = go b xs
    blk b@(Para xs)  = go b xs
    blk b            = return [b]

    go :: (MonadIO m) => Block -> [Inline] -> FilterT m [Block]
    go b xs = do
      ys <- sequence <$> mapM include (filter noBreaks xs)
      return (maybe [b] concat ys)

    noBreaks :: Inline -> Bool
    noBreaks LineBreak = False
    noBreaks Space     = False
    noBreaks SoftBreak = False
    noBreaks _         = True

    include :: (MonadIO m) => Inline -> FilterT m (Maybe [Block])
    include (Str s) = traverse parse (inclusionMarker s)
    include _       = return Nothing

    parse :: (MonadIO m) => FileRef -> FilterT m [Block]
    parse (FileRef f hid) = do
      verbose ("including markdown file: " ++ f)
      file <- realpath f
      addDependency file
      (Pandoc _ bs) <- processFile file

      case hid of
        Nothing  -> return bs
        Just id' -> return (narrowToHeader id' bs)
