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
import Data.Version (showVersion)
import qualified GHC.IO.Encoding as E
import Options.Applicative

--------------------------------------------------------------------------------
-- Project imports.
import Paths_edify (version)

--------------------------------------------------------------------------------
-- Commands from files in this directory.
import qualified Build
import qualified Filter
import qualified Outline
import qualified Stitch

--------------------------------------------------------------------------------
-- | Type for the command line parser.
data Command = BuildC Build.Options
             | FilterC Filter.Options
             | OutlineC Outline.Options
             | StitchC Stitch.Options

--------------------------------------------------------------------------------
-- | Command line parser.
parser :: Parser Command
parser = subparser $ mconcat
    [ command "build"   (info (helper <*> buildCmd)   (progDesc buildDesc))
    , command "filter"  (info (helper <*> filterCmd)  (progDesc filterDesc))
    , command "outline" (info (helper <*> outlineCmd) (progDesc outlineDesc))
    , command "stitch"  (info (helper <*> stitchCmd)  (progDesc stitchDesc))
    ]
  where
    buildCmd   = BuildC   <$> Build.options
    filterCmd  = FilterC  <$> Filter.options
    outlineCmd = OutlineC <$> Outline.options
    stitchCmd  = StitchC  <$> Stitch.options

    buildDesc   = "Build an entire Markdown project"
    filterDesc  = "Filter Pandoc JSON content from STDIN to STDOUT"
    outlineDesc = "Print the heading outline of a Markdown document"
    stitchDesc  = "Stitch together files listed in FILE"

--------------------------------------------------------------------------------
-- | Print the version number and exit.
versionCmd :: Parser (a -> a)
versionCmd = infoOption versionStr versionMod
  where
    versionStr = showVersion version -- From cabal file.
    versionMod = long "version" <> hidden <>
                 help "Print version number and exit"

--------------------------------------------------------------------------------
-- | Dispatch the subcommand to the correct function.
dispatch :: Command -> IO ()
dispatch (BuildC options)   = Build.dispatch options
dispatch (FilterC options)  = Filter.dispatch options
dispatch (OutlineC options) = Outline.dispatch options
dispatch (StitchC options)  = Stitch.dispatch options

--------------------------------------------------------------------------------
main :: IO ()
main = do
    E.setLocaleEncoding E.utf8
    dispatch =<< execParser opts
  where
    opts = info (helper <*> versionCmd <*> parser) mempty
