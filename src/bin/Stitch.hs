{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Command to work with manifest files.
module Stitch
       ( Options
       , options
       , dispatch
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.File.Manifest (Manifest, files, parseFile)
import Text.Edify.File.Stitch (hStitch)

--------------------------------------------------------------------------------
data Options = Options FilePath String Bool

--------------------------------------------------------------------------------
options :: Parser Options
options = Options <$> argument str (metavar "FILE")
                  <*> strOption delimiter
                  <*> switch listFiles
  where
    delimiter = short 'd' <> long "delimiter" <> metavar "TEXT" <>
                value "\n" <> showDefault <> help "Insert TEXT between files"

    listFiles = short 'l' <> long "list" <>
                help "List files instead of actually stitching"


--------------------------------------------------------------------------------
dispatch :: Options -> IO ()
dispatch (Options file delimiter False) = stitch file delimiter
dispatch (Options file _         True)  = dumpFiles file

--------------------------------------------------------------------------------
-- | Stitch files together and dump to STDOUT.
stitch :: FilePath -> String -> IO ()
stitch file delimiter = loadManifest file go where
  go fileList = hStitch fileList (T.pack delimiter) stdout

--------------------------------------------------------------------------------
dumpFiles :: FilePath -> IO ()
dumpFiles file = loadManifest file go where
  go fileList = mapM_ putTextLn (files fileList)

--------------------------------------------------------------------------------
loadManifest :: FilePath -> (Manifest -> IO ()) -> IO ()
loadManifest file f = go =<< parseFile file where
  go (Left err)       = T.hPutStrLn stderr err
  go (Right fileList) = f fileList
