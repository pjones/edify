{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Extra Pandoc features for dealing with source code.
module Text.Edify.Filter.Insert
       ( insertFile
       ) where

--------------------------------------------------------------------------------
-- | Library imports.
import Control.Applicative
import Control.Exception
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Text.Pandoc

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Util.Indent
import Text.Edify.Util.Narrow

--------------------------------------------------------------------------------
data MissingTokenError = MissingTokenError String String
  deriving (Typeable)

instance Show MissingTokenError where
  show (MissingTokenError file token) =
    "MissingTokenError: " ++ "can't find token '" ++
    token ++ "' in " ++ file

instance Exception MissingTokenError

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
insertFile :: Block -> IO Block
insertFile cb@(CodeBlock (blkid, classes, alist) _) =
  case lookup "insert" alist <|> lookup "include" alist of
    Just f  -> return . CodeBlock (blkid, classes, alist) =<< newtxt f
    Nothing -> return cb
  where newtxt f = readCodeFile f classes (lookup "token" alist)
insertFile x = return x

--------------------------------------------------------------------------------
-- | Read a file with code in it, possibly narrowing to a token.
readCodeFile :: FilePath -> [String] -> Maybe String -> IO String
readCodeFile path _ tokenM = case tokenM of
  Nothing    -> transformS <$> readFile path
  Just token -> do
    contents <- T.readFile path
    case narrow (T.pack token) contents of
      Nothing  -> throwIO (MissingTokenError path token)
      Just txt -> (return . T.unpack . transformT) txt

  where
    transformS :: String -> String
    transformS = T.unpack . transformT . T.pack

    transformT :: Text -> Text
--    transformT = specialComments path lang . removeIndent
    transformT = removeIndent

    -- lang :: String
    -- lang = maybe "" id (listToMaybe classes)
