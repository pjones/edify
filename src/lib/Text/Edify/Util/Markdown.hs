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
  , defaultPandocExtensions
  , toPandocExtensions
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
defaultPandocExtensions :: Pandoc.Extensions
defaultPandocExtensions = mconcat
  [ Pandoc.pandocExtensions
  , Pandoc.githubMarkdownExtensions
  ]

--------------------------------------------------------------------------------
-- | Turn a list of extension names into the 'Extension' type by
-- prepending @Ext_@ and reading the type.
toPandocExtensions :: [Text] -> Pandoc.Extensions
toPandocExtensions ts =
  defaultPandocExtensions
    <> Pandoc.extensionsFromList (mapMaybe toExt ts)
  where
    toExt :: Text -> Maybe Pandoc.Extension
    toExt = ("Ext_" <>) >>> toString >>> readMaybe

--------------------------------------------------------------------------------
readerOptions :: Pandoc.Extensions -> ReaderOptions
readerOptions ext =
  def { readerStandalone = True
      , readerExtensions = ext
      }

--------------------------------------------------------------------------------
writerOptions :: Pandoc.Extensions -> WriterOptions
writerOptions ext =
  def { writerExtensions = ext
      }

--------------------------------------------------------------------------------
readMarkdownText :: Pandoc.Extensions -> Text -> Either Text Pandoc
readMarkdownText e t =
  case Pandoc.runPure $ readMarkdown (readerOptions e) t of
    Left e  -> Left (show e)
    Right p -> Right p

--------------------------------------------------------------------------------
readMarkdownFile
  :: MonadIO m
  => Pandoc.Extensions
  -> FilePath
  -> m (Either Text Pandoc)
readMarkdownFile ext path =
  readFileText path <&> readMarkdownText ext

--------------------------------------------------------------------------------
writeMarkdownFile
  :: MonadIO m
  => Pandoc.Extensions
  -> Pandoc
  -> FilePath
  -> m (Maybe Text)
writeMarkdownFile ext doc file = liftIO $ do
    result <- Pandoc.runIO $ do
      template <- Pandoc.compileDefaultTemplate "markdown"
      let opts = (writerOptions ext) { writerTemplate = Just template}
      writeMarkdown opts doc

    case result of
      Left e  -> return (Just $ show e)
      Right t -> Text.writeFile file t >> return Nothing
