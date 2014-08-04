{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
-- Library imports.
import           Data.Monoid
import qualified Data.Text as T
import           Data.Version (showVersion)
import           Options.Applicative
import           System.IO
import           Text.Pandoc.JSON

--------------------------------------------------------------------------------
-- Project imports.
import Paths_edify (version)
import Text.Edify.File.Manifest (Manifest, files, parseFile)
import Text.Edify.File.Stitch (hStitch)
import Text.Edify.Filter (filterPandoc)

--------------------------------------------------------------------------------
-- | Type for the command line parser.
data Command = Filter | Stitch FilePath String Bool
  deriving Show

--------------------------------------------------------------------------------
-- | Command line parser.
parser :: Parser Command
parser = subparser $ mconcat
    [ command "filter" (info (helper <*> (pure Filter)) (progDesc filterDesc))
    , command "stitch" (info (helper <*> stitchCmd)     (progDesc stitchDesc))
    ]
  where
    filterDesc = "Filter Pandoc JSON content from STDIN to STDOUT"
    stitchDesc = "Stitch together files listed in FILE"

--------------------------------------------------------------------------------
-- | The "stitch" Command.
stitchCmd :: Parser Command
stitchCmd = Stitch <$> argument str (metavar "FILE")
                   <*> strOption delimiter
                   <*> switch listFiles
  where
    delimiter = short 'd' <> long "delimiter" <> metavar "TEXT" <>
                value "\n" <> showDefault <> help "Insert TEXT between files"

    listFiles = short 'l' <> long "list" <>
                help "List files instead of actually stitching"

--------------------------------------------------------------------------------
-- | Print the version number and exit.
versionCmd :: Parser (a -> a)
versionCmd = infoOption versionStr versionMod
  where
    versionStr = showVersion version -- From cabal file.
    versionMod = long "version" <> hidden <>
                 help "Print version number and exit"

--------------------------------------------------------------------------------
loadManifest :: FilePath -> (Manifest -> IO ()) -> IO ()
loadManifest file f = go =<< parseFile file where
  go (Left err)       = hPutStrLn stderr err
  go (Right fileList) = f fileList

--------------------------------------------------------------------------------
-- | Stitch files together and dump to STDOUT.
stitch :: FilePath -> String -> IO ()
stitch file delimiter = loadManifest file go where
  go fileList = hStitch fileList (T.pack delimiter) stdout

--------------------------------------------------------------------------------
dumpFiles :: FilePath -> IO ()
dumpFiles file = loadManifest file go where
  go fileList = mapM_ (putStrLn . T.unpack) (files fileList)

--------------------------------------------------------------------------------
-- | Dispatch the subcommand to the correct function.
dispatch :: Command -> IO ()
dispatch (Filter) = toJSONFilter filterPandoc
dispatch (Stitch file delimiter False) = stitch file delimiter
dispatch (Stitch file _         True)  = dumpFiles file

--------------------------------------------------------------------------------
main :: IO ()
main = dispatch =<< execParser opts
  where opts = info (helper <*> versionCmd <*> parser) mempty
