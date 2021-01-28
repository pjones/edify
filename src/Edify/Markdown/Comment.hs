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
module Edify.Markdown.Comment
  ( Comment (..),
    commentP,
    commentT,
  )
where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text.Lazy.Builder as Builder
import Edify.JSON

-- | HTML comments inside Markdown documents.
--
-- @since 0.5.0.0
newtype Comment = Comment
  {unComment :: Text}
  deriving stock (Generic, Show, Eq)
  deriving newtype (ToJSON, FromJSON, Semigroup, Monoid)

-- | Comment parser.
--
-- @since 0.5.0.0
commentP :: Atto.Parser Comment
commentP = do
  Atto.skipWhile Atto.isHorizontalSpace
  _ <- Atto.string "<!--"
  body <- Atto.manyTill Atto.anyChar (Atto.string "-->")
  pure (Comment $ toText body)

-- | Convert a comment back into text.
--
-- @since 0.5.0.0
commentT :: Comment -> Builder.Builder
commentT (Comment body) =
  mconcat
    [ "<!--",
      Builder.fromText body,
      "-->"
    ]
