{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Parse a file that lists file names and return the file names.
-- Supports Unix-style comments.
module Text.Edify.File.Manifest (Manifest, files, parseFile) where

--------------------------------------------------------------------------------
-- Library imports.
import Data.Attoparsec.Text hiding (parse)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Project imports.
import qualified Text.Edify.Util.Parse as EP

--------------------------------------------------------------------------------
-- | Wrapper around a list of file names.
newtype Manifest = Manifest
  { files :: [Text] -- ^ List of file names.
  } deriving Show

--------------------------------------------------------------------------------
-- | Tokens that can be parsed.
data Token = Comment | FileName Text

--------------------------------------------------------------------------------
-- | Extracts file names from a list of tokens.
onlyFiles :: [Token] -> [Text]
onlyFiles = concatMap extract . filter predicate where
  predicate :: Token -> Bool
  predicate Comment      = False
  predicate (FileName _) = True

  extract :: Token -> [Text]
  extract Comment      = []
  extract (FileName x) = [x]

--------------------------------------------------------------------------------
-- | Parse a file and return a 'Manifest'.
parseFile :: FilePath -> IO (Either Text Manifest)
parseFile = EP.parseFile fileList

--------------------------------------------------------------------------------
-- | Parses a list of file names.
fileList :: Parser Manifest
fileList = Manifest . onlyFiles <$>
           manyTill (whitespace <|> comment <|> fileName) endOfInput

--------------------------------------------------------------------------------
-- | Parse a file name.
fileName :: Parser Token
fileName = do first  <- anyChar
              others <- manyTill anyChar endFileName
              return . FileName . T.strip . T.pack $ (first : others)
           <?> "file name"
  where
    endFileName :: Parser ()
    endFileName = do
      c <- peekChar'
      if c == '\n' || c == '#'
        then pass
        else mzero

--------------------------------------------------------------------------------
-- | Parse out whitespace as a comment.
whitespace :: Parser Token
whitespace = EP.parseWhitespace >> return Comment

--------------------------------------------------------------------------------
-- | Parse comments.
comment :: Parser Token
comment = EP.parseComment >> return Comment
