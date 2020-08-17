-- |
--
-- Copyright:
--   This file is part of the package Edify. It is subject to the
--   license terms in the LICENSE file found in the top-level
--   directory of this distribution and at:
--
--     https://github.com/pjones/edify
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the terms
--   contained in the LICENSE file.
--
-- License: Apache-2.0
module Main
  ( main,
  )
where

import qualified Edify.Command.Chunk as Chunk
import qualified Edify.Command.Narrow as Narrow
import qualified Edify.Command.Source as Source
import qualified Options.Applicative as Options

-- import Data.Version (showVersion)
-- import Paths_edify (version)

-- | Type for the command line parser.
data Command
  = NarrowC Narrow.Flags
  | ChunkC Chunk.Actions
  | SourceC Source.Flags

-- | Command line parser.
commands :: Options.Parser Command
commands =
  Options.hsubparser
    ( mconcat
        [ mkcmd "narrow" NarrowC Narrow.command,
          mkcmd "chunk" ChunkC Chunk.command,
          mkcmd "source" SourceC Source.command
        ]
    )
  where
    mkcmd name ctor (desc, flags) =
      Options.command
        name
        ( Options.info
            (ctor <$> flags)
            (Options.progDesc desc)
        )

-- -- | Print the version number and exit.
-- versionCmd :: Parser (a -> a)
-- versionCmd = infoOption versionStr versionMod
--   where
--     versionStr = showVersion version -- From cabal file.
--     versionMod =
--       long "version" <> hidden
--         <> help "Print version number and exit"

main :: IO ()
main =
  Options.execParser opts >>= \case
    NarrowC flags -> Narrow.main flags
    ChunkC actions -> Chunk.main actions
    SourceC flags -> Source.main flags
  where
    opts = Options.info (Options.helper <*> commands) mempty
