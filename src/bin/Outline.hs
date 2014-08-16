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
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Options.Applicative
import           System.IO
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown

--------------------------------------------------------------------------------
-- Project imports.
import qualified Text.Edify.File.Time as ET
import           Text.Edify.Time.TimeCode
import           Text.Edify.Time.TimeTree
import           Text.Edify.Util.HeaderTree

--------------------------------------------------------------------------------
-- | Options for the outline command.
data Options = Options
  { file     :: Maybe FilePath -- ^ File to read (or STDIN).
  , timeKey  :: Maybe String   -- ^ Time codes from header attribute.
  , timeFile :: Maybe FilePath -- ^ Time codes from a file.
  }

--------------------------------------------------------------------------------
-- | Command line parser for the outline command.
options :: Parser Options
options = Options <$> (optional $ argument str (metavar "FILE"))
                  <*> (optional $ strOption timeKeyOpt)
                  <*> (optional $ strOption timeFileOpt)
  where
    timeKeyOpt = short 'a' <> long "time-attr" <> metavar "KEY" <>
                 help "Read time codes from header attribute KEY"

    timeFileOpt = short 'f' <> long "time-file" <> metavar "FILE" <>
                  help "Read time codes from a file"

--------------------------------------------------------------------------------
dispatch :: Options -> IO ()
dispatch Options{..} = do
  markdown <- content file
  treeM    <- runMaybeT (msum $ map ($ (headers markdown)) timeCodeOptions)

  case treeM of
    Nothing   -> simpleOutline 0 (headers markdown)
    Just tree -> timeOutline tree

  where
    timeCodeOptions :: [[HeaderTree] -> MaybeT IO [TimeTree]]
    timeCodeOptions = [usingBoth, usingAttr, usingMap]

    usingBoth :: [HeaderTree] -> MaybeT IO [TimeTree]
    usingBoth hs = do
      key   <- maybe mzero return timeKey
      mFile <- maybe mzero return timeFile
      dictE <- liftIO (ET.parseFile mFile)

      either (liftIO . fail) return $ do
        dict <- dictE
        timeTreeFromMapOrAttr dict key hs

    usingAttr :: [HeaderTree] -> MaybeT IO [TimeTree]
    usingAttr hs = do
      key <- maybe mzero return timeKey
      either (liftIO . fail) return $ timeTreeFromAttr key hs

    usingMap :: [HeaderTree] -> MaybeT IO [TimeTree]
    usingMap hs = do
      mFile <- maybe mzero return timeFile
      m     <- liftIO (ET.parseFile mFile)
      either (liftIO . fail) return $ flip timeTreeFromMap hs =<< m

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
  topLevelTotal <- sum <$> mapM (\t -> printTree 0 t <* putStr "\n") ts
  printTitle 0 "[TOTAL]" '-' topLevelTotal

--------------------------------------------------------------------------------
printTree :: Int -> TimeTree -> IO TimeCode
printTree n (TimeTree hi timeCode hs) = do
    printTitle n (title hi) '.' timeCode
    mapM_ (printTree (n+2)) hs
    return timeCode

--------------------------------------------------------------------------------
printTitle :: Int -> String -> Char -> TimeCode -> IO ()
printTitle n title lineChar tc = putStrLn line where
  title' = replicate n ' ' ++ take (titleWidth - n) title
  bar    = replicate (titleWidth - length title' - 2) lineChar
  line   = title' ++ " " ++ bar ++ " " ++ timeCodeStr tc

--------------------------------------------------------------------------------
titleWidth :: Int
titleWidth = 72 -- 80 columns minus time code.

--------------------------------------------------------------------------------
timeCodeStr :: TimeCode -> String
timeCodeStr = T.unpack . asHHMMSS
