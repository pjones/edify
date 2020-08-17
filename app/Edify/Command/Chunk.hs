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
module Edify.Command.Chunk
  ( Actions,
    command,
    main,
  )
where

import qualified Byline.Exit as B
import qualified Data.Aeson as Aeson
import qualified Edify.Format.Markdown as Markdown
import qualified Edify.Input as Input
import qualified Edify.Project.Source as Source
import qualified Edify.Text.Inclusion as Inclusion
import qualified Options.Applicative as O

-- | Actions that this command supports.
--
-- @since 0.5.0.0
data Actions
  = ChunkToJSON (Maybe FilePath)
  | SourceToJSON (Maybe FilePath)
  | MarkdownToJSON (Maybe FilePath)

-- | Errors that need to be handled.
--
-- @since 0.5.0.0
data Error
  = InputError Input.Error
  | InclusionError Inclusion.Error
  | SourceError Source.Error
  | MarkdownError Markdown.Error
  deriving (Generic)

instance B.ToStylizedText Error where
  toStylizedText = \case
    InputError e -> B.toStylizedText e
    InclusionError e -> B.toStylizedText e
    SourceError e -> B.toStylizedText e
    MarkdownError e -> B.toStylizedText e

-- | Command line parser.
--
-- @since 0.5.0.0
command :: (String, O.Parser Actions)
command = ("Parse a file and produce chunked JSON output", actions)
  where
    actions :: O.Parser Actions
    actions =
      asum
        [ (ChunkToJSON <$> optionalFile)
            <* O.flag'
              ()
              ( mconcat
                  [ O.long "chunks",
                    O.short 'c',
                    O.help "Produce a non-recursive list of chunks"
                  ]
              ),
          (SourceToJSON <$> optionalFile)
            <* O.flag'
              ()
              ( mconcat
                  [ O.long "source",
                    O.short 's',
                    O.help "Produce a list of resolved sources"
                  ]
              ),
          ( MarkdownToJSON <$> optionalFile
              <* O.flag'
                ()
                ( mconcat
                    [ O.long "markdown",
                      O.short 'm',
                      O.help "Chunk a Markdown file"
                    ]
                )
          )
        ]
    optionalFile =
      optional
        ( O.strArgument $
            mconcat
              [ O.metavar "FILE",
                O.help "Process FILE or stdin"
              ]
        )

-- | Turn a file into chucks then write JSON to stdout.
--
-- @since 0.5.0.0
toChunkJSON :: Maybe FilePath -> IO ()
toChunkJSON file = go >>= either B.die putLBSLn
  where
    go :: IO (Either Error LByteString)
    go =
      Input.readInput (Input.filePathToInput file)
        <&> ( first InputError
                >=> (toStrict >>> Inclusion.toChunks)
                >>> bimap InclusionError Aeson.encode
            )

-- | Produce list of 'Source' chunks.
--
-- @since 0.5.0.0
toSourceJSON :: Maybe FilePath -> IO ()
toSourceJSON =
  Input.filePathToInput
    >>> Source.toSource
    >=> either B.die (fst >>> Aeson.encode >>> putLBSLn)

-- | Convert a markdown file to JSON.
--
-- @since 0.5.0.0
toMarkdownJSON :: Maybe FilePath -> IO ()
toMarkdownJSON =
  Input.filePathToInput
    >>> Input.readInput
    >=> either B.die (Markdown.toChunks >>> pure)
    >=> either B.die (Aeson.encode >>> putLBSLn)

-- | Main.
main :: Actions -> IO ()
main = \case
  ChunkToJSON file -> toChunkJSON file
  SourceToJSON file -> toSourceJSON file
  MarkdownToJSON file -> toMarkdownJSON file
