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
import Control.Applicative hiding ((<|>), many)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Parsec hiding (parse)
import Text.Parsec.Text

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
parseFile :: FilePath -> IO (Either String Manifest)
parseFile file = (convert . parse) <$> T.readFile file where
  -- Parse the contents of a file.
  parse :: Text -> Either ParseError Manifest
  parse = runParser fileList () file

  -- Convert the parsec error to a string.
  convert :: Either ParseError Manifest -> Either String Manifest
  convert = either (Left . show) Right

--------------------------------------------------------------------------------
-- | Parses a list of file names.
fileList :: Parser Manifest
fileList = Manifest . onlyFiles <$>
           manyTill (whitespace <|> comment <|> fileName) eof

--------------------------------------------------------------------------------
-- | Parse a file name.
fileName :: Parser Token
fileName = do first  <- anyChar
              others <- manyTill anyChar endFileName
              return . FileName . T.strip . T.pack $ (first : others)
           <?> "file name"
  where
    endFileName :: Parser Char
    endFileName = lookAhead (newline <|> char '#')

--------------------------------------------------------------------------------
-- | Parse out whitespace as a comment.
whitespace :: Parser Token
whitespace = many1 space >> return Comment

--------------------------------------------------------------------------------
-- | Parse comments.
comment :: Parser Token
comment = char '#' >> manyTill anyChar newline >> return Comment <?> "comment"
