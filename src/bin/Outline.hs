{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Command to produce heading outlines from Markdown files.
module Outline
       ( Options
       , options
       , dispatch
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import           Data.Monoid
import qualified Data.Text as T
import           Options.Applicative
import           System.IO
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Time.TimeCode
import Text.Edify.Time.TimeTree
import Text.Edify.Util.HeaderTree

--------------------------------------------------------------------------------
-- | Options for the outline command.
data Options = Options
  { file    :: Maybe FilePath -- ^ File to read (or STDIN).
  , timeKey :: Maybe String   -- ^ Time code from header attribute.
  }

--------------------------------------------------------------------------------
-- | Command line parser for the outline command.
options :: Parser Options
options = Options <$> (optional $ argument str (metavar "FILE"))
                  <*> (optional $ strOption timeKey)
  where
    timeKey = short 'a' <> long "time-attr" <> metavar "KEY" <>
              help "Read time codes from header attribute KEY"

--------------------------------------------------------------------------------
dispatch :: Options -> IO ()
dispatch Options{..} = do
  markdown <- content file

  case timeKey of
    Nothing  -> simpleOutline 0 (headers markdown)
    Just key -> case timeTree key (headers markdown) of
      Left e     -> fail e
      Right tree -> timeOutline tree

--------------------------------------------------------------------------------
content :: Maybe FilePath -> IO String
content file = case file of
  Nothing -> hGetContents stdin
  Just f  -> readFile f

--------------------------------------------------------------------------------
headers :: String -> [HeaderTree]
headers = headerTree . readMarkdown def

--------------------------------------------------------------------------------
simpleOutline :: Int -> [HeaderTree] -> IO ()
simpleOutline _ []                     = return ()
simpleOutline n ((HeaderTree h hs):xs) = do
  putStrLn (replicate n ' ' ++ title h)
  simpleOutline (n+2) hs
  simpleOutline n xs

--------------------------------------------------------------------------------
timeOutline :: [TimeTree] -> IO ()
timeOutline [] = return ()
timeOutline ts = do
  topLevelTotal <- sum <$> mapM (printTree 0) ts
  putStrLn (replicate (titleWidth - 1) '-' ++ " " ++ timeCodeStr topLevelTotal)

--------------------------------------------------------------------------------
printTree :: Int -> TimeTree -> IO TimeCode
printTree n (TimeTree hi timeCode hs) = do
    putStrLn (title' ++ " " ++ dots ++ " " ++ timeCodeStr timeCode)
    mapM_ (printTree (n+2)) hs
    return timeCode
  where
    title' = replicate n ' ' ++ take (titleWidth - n) (title hi)
    dots   = replicate (titleWidth - length title' - 2) '.'

--------------------------------------------------------------------------------
titleWidth :: Int
titleWidth = 72 -- 80 columns minus time code.

--------------------------------------------------------------------------------
timeCodeStr :: TimeCode -> String
timeCodeStr = T.unpack . asHHMMSS
