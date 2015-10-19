{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Types and functions to support time codes read from a file.
module Text.Edify.File.Time
       ( parseFile
       , parser
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import           Data.Char
import           Data.Map (Map)
import qualified Data.Map as M
import           Text.Parsec hiding (parse)
import           Text.Parsec.Text

--------------------------------------------------------------------------------
-- Project imports.
import           Text.Edify.Time.TimeCode
import qualified Text.Edify.Util.Parse as EP

--------------------------------------------------------------------------------
-- | Internal type that the parser functions use.
data Token = Comment | Entry String TimeCode

--------------------------------------------------------------------------------
-- | Parse time codes from a file.
parseFile :: FilePath -> IO (Either String (Map String TimeCode))
parseFile = EP.parseFile parser

--------------------------------------------------------------------------------
-- | Raw parser for mapping identifiers to time codes.
parser :: Parser (Map String TimeCode)
parser = tokensToMap <$> manyTill (whitespace <|> comment <|> entry) eof
  where
    whitespace :: Parser Token
    whitespace = EP.parseWhitespace >> return Comment

    comment :: Parser Token
    comment = EP.parseComment >> return Comment

--------------------------------------------------------------------------------
-- | Parse an entry that is made up of an identifier and then a time code.
entry :: Parser Token
entry = do
    first  <- notSpace <?> "time identifier"
    others <- manyTill anyChar space <?> "time identifier"
    tc     <- spaces >> parseTimeCode
    return $ Entry (first:others) tc
  where
    notSpace :: Parser Char
    notSpace = satisfy (not . isSpace)

--------------------------------------------------------------------------------
-- | Convert tokens into a mapping of time codes.
tokensToMap :: [Token] -> Map String TimeCode
tokensToMap = foldr insert M.empty
  where
    insert :: Token -> Map String TimeCode -> Map String TimeCode
    insert (Comment) m         = m
    insert (Entry key value) m = M.insert key value m
