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
module Edify.Command.Source
  ( Flags,
    command,
    main,
  )
where

import qualified Byline.Exit as B
import qualified Data.Text.Lazy.Builder as LTB
import qualified Edify.Input as Input
import qualified Edify.Project.Source as Source
import qualified Options.Applicative as O

-- | Command line flags.
--
-- @since 0.5.0.0
newtype Flags = Flags
  { flagsFile :: Maybe FilePath
  }

-- | Command description and parser.
--
-- @since 0.5.0.0
command :: (String, O.Parser Flags)
command = ("Fully process a source file", flags)
  where
    flags :: O.Parser Flags
    flags =
      Flags
        <$> optional
          ( O.argument
              O.str
              ( mconcat
                  [ O.metavar "FILE",
                    O.help "Read FILE, or stdin"
                  ]
              )
          )

-- | Main entry point for the source command.
--
-- @since 0.5.0.0
main :: Flags -> IO ()
main Flags {..} =
  Input.filePathToInput flagsFile & Source.toSource
    >>= either
      B.die
      ( fst
          >>> Source.fromSource
          >>> snd
          >>> LTB.toLazyText
          >>> putLTextLn
      )
