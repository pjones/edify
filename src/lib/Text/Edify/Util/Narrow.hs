{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
module Text.Edify.Util.Narrow
       ( Markers (..)
       , Token   (..)
       , narrow
       , narrowToToken
       , narrowToHeader
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Definition (Block(..))
import Text.Parsec hiding (token)
import Text.Parsec.Text

--------------------------------------------------------------------------------
-- | Beginning and ending markers for narrowing.
data Markers = Markers Text Text

--------------------------------------------------------------------------------
-- | A bit of text that identifies a specific marker.
data Token = Token Text

--------------------------------------------------------------------------------
-- | Narrow the given text using the default token delimiters.
narrow :: Token -> Text -> Either String Text
narrow = narrowToToken (Markers "<<:" ":>>")

--------------------------------------------------------------------------------
-- | Narrow the given text to a beginning and ending delimiter.
narrowToToken :: Markers -> Token -> Text -> Either String Text
narrowToToken m t input =
  case runParser (narrowP m t) () "" input of
    Left e     -> Left (show e)
    Right text -> Right text

--------------------------------------------------------------------------------
-- | Extract the text between markers.
narrowP :: Markers -> Token -> Parser Text
narrowP (Markers start end) (Token token) = do
  let beginning = string (T.unpack start) >> spaces >>
                  string (T.unpack token) >> lookAhead space

  -- Skip over all characters until we hit the starting marker and the
  -- token.  Then record what comes after the token.  This is useful
  -- for languages like CSS that don't have single-line comments.
  void (manyTill anyChar (try beginning) <?> "opening marker")
  after <- manyTill anyChar endOfLine

  let ending = string (T.unpack end) >> spaces >>
               string (T.unpack . T.strip . T.pack $ after)

  -- Now fetch all the characters *before* then ending marker.  Also
  -- skip the entire line that the ending marker is on.
  text <- manyTill anyChar (try ending) <?> "closing marker"
  return (T.dropWhileEnd (/= '\n') . T.pack $ text)

--------------------------------------------------------------------------------
-- | State for searching a document, looking for all content falling
-- under a header.
data NarrowState = Found [Block] | Searching Block [Block]

--------------------------------------------------------------------------------
-- | Restrict the given list of blocks to only include those that fall
-- under the header with the given ID.
narrowToHeader :: String -> [Block] -> [Block]
narrowToHeader _      [] = []
narrowToHeader headid xs =
  case start xs of
    []     -> []
    h:rest -> case end h rest of
                Found bs       -> h:bs
                Searching _ bs -> h:bs

  where
    start :: [Block] -> [Block]
    start = dropWhile (not . isMatch)

    end :: Block -> [Block] -> NarrowState
    end h@Header{} bs = foldl collect (Searching h []) bs
    end h          _  = Searching h []

    isMatch :: Block -> Bool
    isMatch (Header _ (sid, _, _) _) = sid == headid
    isMatch _                        = False

    collect :: NarrowState -> Block -> NarrowState
    collect state@(Found _)  _   = state
    collect (Searching h bs) blk = if below h blk
                                     then Searching h (bs ++ [blk])
                                     else Found bs

    below :: Block -> Block -> Bool
    below (Header n _ _) (Header n' _ _) = n' > n
    below _ _ = True
