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
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (traverse)
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
    Just f  -> CodeBlock (blkid, classes, alist) <$> newtxt f
    Nothing -> return cb
  where newtxt f = readCodeFile f (lookup "token" alist)
insertFile x = return x

--------------------------------------------------------------------------------
-- | Read a file with code in it, possibly narrowing to a token.
readCodeFile :: (MonadIO m) => FilePath -> Maybe String -> FilterT m String
readCodeFile path Nothing      = realpath path >>= liftIO . readFile
readCodeFile path (Just token) = do
  cleanPath <- realpath path
  addDependency cleanPath
  contents <- liftIO (T.readFile cleanPath)

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
insertParsedFile = fmap concat . mapM go

  where
    go :: (MonadIO m) => Block -> FilterT m [Block]
    go block = do
      let update xs = do mblk <- include xs
                         case mblk of
                           Nothing -> return [block]
                           Just bs -> return bs
      case block of
        Plain xs -> update xs
        Para  xs -> update xs
        _        -> return [block]

    include :: (MonadIO m) => [Inline] -> FilterT m (Maybe [Block])
    include [Str s] = traverse parse (inclusionMarker s)
    include _       = return Nothing

    parse :: (MonadIO m) => FilePath -> FilterT m [Block]
    parse f = do file <- realpath f
                 addDependency file
                 (Pandoc _ bs) <- processFile file
                 return bs
