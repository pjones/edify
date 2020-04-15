{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Helper functions for parsing.
module Text.Edify.Util.Parse
       ( parseFile
       , parseWhitespace
       , parseComment
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import Data.Attoparsec.Text hiding (parse)

--------------------------------------------------------------------------------
-- | Parse a file and return the result.
parseFile :: Parser a -> FilePath -> IO (Either Text a)
parseFile parser file = convert . parse <$> readFileText file
  where parse   = parseOnly parser
        convert = either (Left . show) Right

--------------------------------------------------------------------------------
-- | Skip whitespace.  There must be at least one whitespace character
-- or this parser will fail.
parseWhitespace :: Parser ()
parseWhitespace = many1 space $> ()

--------------------------------------------------------------------------------
-- | Parse comments.
parseComment :: Parser Text
parseComment = (char '#' >> toText <$> manyTill anyChar endOfLine) <?> "comment"
