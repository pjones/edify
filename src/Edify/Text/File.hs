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
module Edify.Text.File
  ( Config (..),
    Input (..),
    Error (..),
    processInput,
  )
where

import qualified Byline as B
import Edify.Input (Input)
import qualified Edify.Input as Input
import qualified Edify.Text.Indent as Indent
import qualified Edify.Text.Narrow as Narrow

-- | How to process a file.
--
-- @since 0.5.0.0
data Config = Config
  { -- | If a token is give, narrow the input file.
    configNarrow :: Maybe Narrow.Token,
    -- | Whether or not to remove leading indentation.
    configStripIndentation :: Bool
  }

-- | Errors that may occur while processing a file.
--
-- @since 0.5.0.0
data Error
  = -- | A error getting input.
    InputError Input.Error
  | -- | An error occured while narrwing the text.
    NarrowError Input Narrow.Error
  deriving (Generic, Show)

instance B.ToStylizedText Error where
  toStylizedText = \case
    NarrowError input e ->
      mconcat
        [ "while processing ",
          B.toStylizedText input,
          ": ",
          B.toStylizedText e
        ]
    InputError e ->
      B.toStylizedText e

-- | Read 'Text' from the given 'Input' and process it according to
-- 'Config'.
--
-- @since 0.5.0.0
processInput :: MonadIO m => Config -> Input -> m (Either Error Text)
processInput Config {..} input =
  Input.readInput input
    <&> ( first InputError
            >=> (toStrict >>> narrow)
            >=> stripIndentation
        )
  where
    narrow :: Text -> Either Error Text
    narrow text = case configNarrow of
      Nothing ->
        pure text
      Just token ->
        Narrow.narrow token text
          & first (NarrowError input)
    stripIndentation :: Text -> Either Error Text
    stripIndentation text
      | configStripIndentation = pure (Indent.stripLeadingIndent text)
      | otherwise = pure text
