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
import Control.Monad (msum)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Options.Applicative
import Text.Pandoc.Definition (Pandoc(..))

--------------------------------------------------------------------------------
-- Project imports.
import qualified Text.Edify.File.Time as ET
import Text.Edify.Time.TimeCode
import Text.Edify.Time.TimeTree
import Text.Edify.Util.HeaderTree
import Text.Edify.Util.Markdown (readMarkdownText)

--------------------------------------------------------------------------------
-- | Options for the outline command.
data Options = Options
  { file     :: Maybe FilePath -- ^ File to read (or STDIN).
  , timeKey  :: Maybe Text     -- ^ Time codes from header attribute.
  , timeFile :: Maybe FilePath -- ^ Time codes from a file.
  }

--------------------------------------------------------------------------------
-- | Command line parser for the outline command.
options :: Parser Options
options = Options <$> optional (argument str (metavar "FILE"))
                  <*> optional (strOption timeKeyOpt)
                  <*> optional (strOption timeFileOpt)
  where
    timeKeyOpt = short 'a' <> long "time-attr" <> metavar "KEY" <>
                 help "Read time codes from header attribute KEY"

    timeFileOpt = short 'f' <> long "time-file" <> metavar "FILE" <>
                  help "Read time codes from a file"

--------------------------------------------------------------------------------
dispatch :: Options -> IO ()
dispatch Options{..} = do
  markdown <- readFileOrStdin file
  treeM    <- runMaybeT (msum $ map ($ headers markdown) timeCodeOptions)

  case treeM of
    Nothing   -> simpleOutline 0 (headers markdown)
    Just tree -> timeOutline tree

  where
    timeCodeOptions :: [[HeaderTree Text] -> MaybeT IO [TimeTree Text]]
    timeCodeOptions = [usingBoth, usingAttr, usingMap]

    usingBoth :: [HeaderTree Text] -> MaybeT IO [TimeTree Text]
    usingBoth hs = do
      key   <- maybe mzero return timeKey
      mFile <- maybe mzero return timeFile
      dictE <- liftIO (ET.parseFile mFile)

      either (fail . toString) return $ do
        dict <- dictE
        timeTreeFromMapOrAttr dict key hs

    usingAttr :: [HeaderTree Text] -> MaybeT IO [TimeTree Text]
    usingAttr hs = do
      key <- maybe mzero return timeKey
      either (fail . toString) return $ timeTreeFromAttr key hs

    usingMap :: [HeaderTree Text] -> MaybeT IO [TimeTree Text]
    usingMap hs = do
      mFile <- maybe mzero return timeFile
      m     <- liftIO (ET.parseFile mFile)
      either (fail . toString) return $ flip timeTreeFromMap hs =<< m

--------------------------------------------------------------------------------
readFileOrStdin :: Maybe FilePath -> IO Text
readFileOrStdin = maybe Text.getContents readFileText

--------------------------------------------------------------------------------
headers :: Text -> [HeaderTree Text]
headers content = case readMarkdownText content of
  Left _             -> []
  Right (Pandoc _ x) -> headerTree x

--------------------------------------------------------------------------------
simpleOutline :: Int -> [HeaderTree Text] -> IO ()
simpleOutline _ []                     = return ()
simpleOutline n (HeaderTree h hs:xs) = do
  Text.putStrLn (Text.replicate n " " <> title h)
  simpleOutline (n+2) hs
  simpleOutline n xs

--------------------------------------------------------------------------------
timeOutline :: [TimeTree Text] -> IO ()
timeOutline [] = return ()
timeOutline ts = do
  topLevelTotal <- sum <$> mapM (\t -> printTree 0 t <* putStr "\n") ts
  printTitle 0 "[TOTAL]" "-" topLevelTotal

--------------------------------------------------------------------------------
printTree :: Int -> TimeTree Text -> IO TimeCode
printTree n (TimeTree hi timeCode hs) = do
    printTitle n (title hi) "." timeCode
    mapM_ (printTree (n+2)) hs
    return timeCode

--------------------------------------------------------------------------------
printTitle :: Int -> Text -> Text -> TimeCode -> IO ()
printTitle n title lineChar tc = Text.putStrLn line where
  title' = Text.replicate n " " <> Text.take (titleWidth - n) title
  bar    = Text.replicate (titleWidth - Text.length title' - 2) lineChar
  line   = title' <> " " <> bar <> " " <> asHHMMSS tc

--------------------------------------------------------------------------------
titleWidth :: Int
titleWidth = 72 -- 80 columns minus time code.
