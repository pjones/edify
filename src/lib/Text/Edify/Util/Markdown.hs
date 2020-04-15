{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Helper functions for using Pandoc to parse markdown files.
module Text.Edify.Util.Markdown
  ( readerOptions
  , writerOptions
  , readMarkdownText
  , readMarkdownFile
  , writeMarkdownFile
  ) where

--------------------------------------------------------------------------------
import Data.Default (def)
import qualified Data.Text.IO as Text
import qualified Text.Pandoc.Class as Pandoc
import Text.Pandoc.Definition (Pandoc)
import qualified Text.Pandoc.Extensions as Pandoc
import Text.Pandoc.Options (ReaderOptions(..), WriterOptions(..))
import Text.Pandoc.Readers.Markdown (readMarkdown)
import qualified Text.Pandoc.Templates as Pandoc
import Text.Pandoc.Writers.Markdown (writeMarkdown)

--------------------------------------------------------------------------------
readerOptions :: ReaderOptions
readerOptions =
  def { readerStandalone = True
      , readerExtensions = mconcat
                             [ Pandoc.pandocExtensions
                             , Pandoc.githubMarkdownExtensions
                             ]
      }

--------------------------------------------------------------------------------
writerOptions :: WriterOptions
writerOptions =
  def { writerExtensions = mconcat
                           [ Pandoc.pandocExtensions
                           ]
      }

--------------------------------------------------------------------------------
readMarkdownText :: Text -> Either Text Pandoc
readMarkdownText t =
  case Pandoc.runPure $ readMarkdown readerOptions t of
    Left e  -> Left (show e)
    Right p -> Right p

--------------------------------------------------------------------------------
readMarkdownFile :: (MonadIO m) => FilePath -> m (Either Text Pandoc)
readMarkdownFile path = do
  str <- liftIO (Text.readFile path)
  return (readMarkdownText str)

--------------------------------------------------------------------------------
writeMarkdownFile :: (MonadIO m) => Pandoc -> FilePath -> m (Maybe Text)
writeMarkdownFile doc file = liftIO $ do
    result <- Pandoc.runIO $ do
      template <- Pandoc.compileDefaultTemplate "markdown"
      let opts = writerOptions { writerTemplate = Just template}
      writeMarkdown opts doc

    case result of
      Left e  -> return (Just $ show e)
      Right t -> Text.writeFile file t >> return Nothing
