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
module Edify.Command.Allow
  ( Flags,
    desc,
    main,
  )
where

import Control.Lens ((^.))
import qualified Edify.Compiler.Allow as Allow
import qualified Edify.Compiler.User as User
import qualified Options.Applicative as Opt
import qualified System.Directory as Directory

-- | Command line options.
--
-- @since 0.5.0.0
newtype Flags = Flags
  { flagsInputFiles :: NonEmpty FilePath
  }

-- | Command description.
--
-- @since 0.5.0.0
desc :: (String, Opt.Parser Flags)
desc = ("Authorize the use of commands in the specific files", flags)
  where
    flags :: Opt.Parser Flags
    flags =
      Flags
        <$> ( fromList
                <$> some
                  ( Opt.strArgument $
                      mconcat
                        [ Opt.metavar "FILE [FILE ...]",
                          Opt.help "Files to authorize"
                        ]
                  )
            )

-- | Main entry point.
--
-- @since 0.5.0.0
main :: User.User -> Flags -> IO ()
main user Flags {..} =
  traverse Directory.makeAbsolute flagsInputFiles
    >>= Allow.main (user ^. #userCommandAllowDir)
