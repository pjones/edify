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
module Edify.Text.Pretty
  ( putError,
    putNote,
    callout,
    shellLine,
    file,
    command,
    edify,

    -- * Colors
    green,
    red,
    yellow,

    -- * Re-exports
    module Data.Text.Prettyprint.Doc,
    module Data.Text.Prettyprint.Doc.Util,
    module Prettyprinter.Render.Terminal,
  )
where

import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util
import Prettyprinter.Render.Terminal
import qualified System.FilePath as FilePath
import Prelude hiding (group)

-- | Print an error message to standard error.
--
-- @since 0.5.0.0
putError :: MonadIO m => Doc AnsiStyle -> m ()
putError doc =
  let msg = mconcat [red "ERROR: ", doc, hardline]
   in liftIO (hPutDoc stderr msg)

-- | Print a notification to standard error.
--
-- @since 0.5.0.0
putNote :: MonadIO m => Doc AnsiStyle -> m ()
putNote doc =
  let msg = mconcat [green "NOTE: ", doc, hardline]
   in liftIO (hPutDoc stderr msg)

-- | Either a space, or a line continuation.
--
-- @since 0.5.0.0
shellLine :: Doc ann
shellLine = flatAlt (" \\" <> hardline) " "

-- | Force the given doc to be indented surrounded by blank lines.
--
-- @since 0.5.0.0
callout :: Doc ann -> Doc ann
callout doc =
  nest 2 $
    (<> hardline) $
      mconcat
        [ hardline,
          doc
        ]

-- | Render a file name after making it relative to some directory.
--
-- @since 0.5.0.0
file ::
  -- | The directory to make the file relative to.
  Maybe FilePath ->
  -- | The file to render.
  FilePath ->
  -- | The rendered file name.
  Doc ann
file = \case
  Nothing -> pretty
  Just dir -> pretty . FilePath.makeRelative dir

-- | Render a shell command.
--
-- @since 0.5.0.0
command :: Text -> Doc ann
command = hang 2 . group . reflow'
  where
    reflow' =
      Text.words
        >>> map pretty
        >>> intersperse shellLine
        >>> mconcat

-- | Render an Edify command for the user to run.
--
-- @since 0.5.0.0
edify :: Text -> [Doc ann] -> Doc ann
edify cmd args =
  hang 2 $
    group $
      mconcat
        [ "edify ",
          pretty cmd,
          shellLine,
          mconcat $ intersperse shellLine args
        ]

-- | Render the given doc in green.
--
-- @since 0.5.0.0
green :: Doc AnsiStyle -> Doc AnsiStyle
green = annotate (color Green)

-- | Render the given doc in red.
--
-- @since 0.5.0.0
red :: Doc AnsiStyle -> Doc AnsiStyle
red = annotate (color Red)

-- | Render the given doc in yellow.
--
-- @since 0.5.0.0
yellow :: Doc AnsiStyle -> Doc AnsiStyle
yellow = annotate (color Yellow)
