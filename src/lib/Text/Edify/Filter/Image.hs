{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Filters for manipulating references to images.
module Text.Edify.Filter.Image
  ( imageRewrite
  , imageExtension
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Network.URI as URI
import System.FilePath ((</>), (-<.>), makeRelative, isRelative, takeExtension)
import Text.Pandoc.Definition

--------------------------------------------------------------------------------
-- Project Imports:
import Text.Edify.Build.Template (OutputFormat(..))
import Text.Edify.Filter.FilterT

--------------------------------------------------------------------------------
-- | Update the URI for an image so it points to a generated image in
-- the output directory.  If the output directory or project directory
-- are not in the reader environment this filter will be skipped.
imageRewrite :: (MonadIO m) => Inline -> FilterT m Inline
imageRewrite inline =
  case inline of
     Image attr ins (url, title) -> do
       url' <- traverse update (parse $ toString url)
       return (Image attr ins (maybe url toText url', title))
     _ -> return inline

  where
    ----------------------------------------------------------------------------
    -- | Test to see if an image's path is a local file.
    parse :: FilePath -> Maybe FilePath
    parse str = do
      uri <- URI.parseURIReference str
      let schema = URI.uriScheme uri
      guard (null schema || schema == "file:")
      return (URI.uriPath uri)

    ----------------------------------------------------------------------------
    -- | Rewrite image paths so they refer to a generated file in the
    -- output directory.
    update :: (MonadIO m) => FilePath -> FilterT m FilePath
    update path = do
      env <- ask
      absolute <- realpath path

      let dep = fromMaybe path (rewrite env absolute)
      addDependency dep
      return dep

    ----------------------------------------------------------------------------
    -- | Do the actual file path rewriting.
    rewrite :: Env m -> FilePath -> Maybe FilePath
    rewrite Env{..} path = do
      projectDir <- envProjectDirectory
      outDir <- envOutputDirectory

      let relative = makeRelative projectDir path
      guard (isRelative relative)

      case imageExtension (takeExtension path) envFormat of
        Nothing  -> return relative
        Just ext -> return (outDir </> relative -<.> ext)

--------------------------------------------------------------------------------
-- | Decide what extension should be used for generated images.
imageExtension :: String
               -- ^ The original file extension.

               -> OutputFormat
               -- ^ The output format that will eventually be generated.

               -> Maybe String
               -- ^ The new extension to use.  If one can't be
               -- calculated then 'Nothing' is returned instead.

-- When targeting Markdown, keep the original extension.
imageExtension ext Markdown = Just ext

-- For PDF, it's best to use a vector format.  Another PDF works.
imageExtension ".dot" PDF    = Just ".dot.pdf"
imageExtension ".svg" PDF    = Just ".svg.pdf"
imageExtension ".tex" PDF    = Just ".tex.pdf"
imageExtension ".msc" PDF    = Just ".msc.pdf"
imageExtension _      PDF    = Nothing

-- For HTML, PNG works pretty well these days.
imageExtension ".dot" HTML   = Just ".dot.png"
imageExtension ".tex" HTML   = Just ".tex.png"
imageExtension ".svg" HTML   = Just ".svg.png"
imageExtension ".msc" HTML   = Just ".msc.png"
imageExtension _      HTML   = Nothing
