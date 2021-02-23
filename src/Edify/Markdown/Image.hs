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
module Edify.Markdown.Image
  ( Image (..),
    src,
    imageP,
    imageT,
  )
where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text.Lazy.Builder as LTB
import Edify.Markdown.Common (matchingBracketP)
import Edify.Markdown.Link (Destination (..), linkDestP, linkDestT)
import Edify.Text.JSON

-- | A reference to an image.
--
-- @since 0.5.0.0
data Image = Image
  { -- | The link text becomes the @alt@ tag.
    imageAltText :: Text,
    -- | The type of image link used.
    imageSrc :: Destination
  }
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via GenericJSON Image

-- | A traversal over the URL for images.
--
-- @since 0.5.0.0
src :: Applicative f => (Text -> f Text) -> Image -> f Image
src f Image {..} =
  case imageSrc of
    Inline url title -> Image imageAltText . (`Inline` title) <$> f url
    Reference {} -> pure (Image imageAltText imageSrc)

-- | Markdown image reference parser.
--
-- @since 0.5.0.0
imageP :: Atto.Parser Image
imageP = do
  _ <- Atto.char '!'
  alt <- matchingBracketP ('[', ']')
  src <- linkDestP

  pure $
    Image
      { imageAltText = alt,
        imageSrc = src
      }

-- | Render an image reference as text.
--
-- @since 0.5.0.0
imageT :: Image -> LTB.Builder
imageT Image {..} =
  mconcat
    [ LTB.fromText "![",
      LTB.fromText imageAltText,
      LTB.singleton ']',
      linkDestT imageSrc
    ]
