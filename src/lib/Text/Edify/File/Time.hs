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
import Data.Attoparsec.Text hiding (parse)
import Data.Char
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Time.TimeCode
import qualified Text.Edify.Util.Parse as EP

--------------------------------------------------------------------------------
-- | Internal type that the parser functions use.
data Token = Comment | Entry Text TimeCode

--------------------------------------------------------------------------------
-- | Parse time codes from a file.
parseFile :: FilePath -> IO (Either Text (Map Text TimeCode))
parseFile = EP.parseFile parser

--------------------------------------------------------------------------------
-- | Raw parser for mapping identifiers to time codes.
parser :: Parser (Map Text TimeCode)
parser = tokensToMap <$> manyTill (whitespace <|> comment <|> entry) endOfInput
  where
    whitespace :: Parser Token
    whitespace = EP.parseWhitespace $> Comment

    comment :: Parser Token
    comment = EP.parseComment $> Comment

--------------------------------------------------------------------------------
-- | Parse an entry that is made up of an identifier and then a time code.
entry :: Parser Token
entry = do
    first  <- notSpace <?> "time identifier"
    others <- manyTill anyChar space <?> "time identifier"
    tc     <- skipSpace *> parseTimeCode
    return $ Entry (toText (first:others)) tc
  where
    notSpace :: Parser Char
    notSpace = satisfy (not . isSpace)

--------------------------------------------------------------------------------
-- | Convert tokens into a mapping of time codes.
tokensToMap :: [Token] -> Map Text TimeCode
tokensToMap = foldr insert M.empty
  where
    insert :: Token -> Map Text TimeCode -> Map Text TimeCode
    insert Comment m           = m
    insert (Entry key value) m = M.insert key value m
