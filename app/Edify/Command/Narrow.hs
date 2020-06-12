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
module Edify.Command.Narrow
  ( Flags,
    command,
    main,
  )
where

import qualified Data.Text.IO as Text
import qualified Edify.Text.Narrow as Narrow
import qualified Options.Applicative as Options

-- | Command line options.
--
-- @since 0.5.0.0
data Flags = Flags
  { -- | The token to narrow to.
    flagsToken :: Text,
    -- | File to narrow.  'Nothing' means read standard input.
    flagsFile :: Maybe FilePath
  }

-- | Sub-command details, including the command line parser.
--
-- @since 0.5.0.0
command :: (String, Options.Parser Flags)
command = ("Narrow a file to the text between two markers", flags)
  where
    flags :: Options.Parser Flags
    flags =
      Flags
        <$> Options.strOption
          ( mconcat
              [ Options.long "token",
                Options.short 't',
                Options.metavar "TEXT",
                Options.help "The token to narrow to"
              ]
          )
        <*> optional
          ( Options.argument
              Options.str
              ( mconcat
                  [ Options.metavar "FILE",
                    Options.help "Read FILE, or stdin"
                  ]
              )
          )

-- | Main entry point for the narrow command.
--
-- @since 0.5.0.0
main :: Flags -> IO ()
main Flags {..} = do
  contents <- case flagsFile of
    Nothing -> Text.getContents
    Just path -> readFileText path
  case Narrow.narrow (Narrow.Token flagsToken) contents of
    Right result -> putText result
    Left (Narrow.Error err) ->
      die ("failed to narrow text: " <> err)
