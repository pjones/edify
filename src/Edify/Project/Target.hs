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
module Edify.Project.Target
  ( -- * Project Targets
    TargetF (..),
    Target,
    resolve,

    -- * Formats
    Format (..),
    formatExtension,
  )
where

import Control.Monad.Except (throwError)
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Char as Char
import qualified Data.HashSet as HashSet
import Data.List ((\\))
import qualified Data.Text as Text
import qualified Edify.Compiler.FilePath as FilePath
import Edify.JSON
import qualified Edify.Markdown.Attributes as Attrs
import Edify.Project.Error
import qualified Edify.Text.Placeholders as Placeholders

-- | Output formats.
--
-- @since 0.5.0.0
data Format
  = -- | Portal Document Format.
    PDF
  | -- | Hyper Text Markup Language.
    HTML
  deriving stock (Generic, Eq, Bounded, Enum, Show)
  deriving (ToJSON, FromJSON) via GenericJSON Format

-- | Generate a file extension for the given 'Project.Format'.
--
-- @since 0.5.0.0
formatExtension :: Format -> FilePath.Ext
formatExtension = \case
  PDF -> FilePath.Ext "pdf"
  HTML -> FilePath.Ext "html"

-- | Description for documents to build out of Markdown.
--
-- @since 0.5.0.0
data TargetF (f :: Readiness) = Target
  { -- | A name for this target.  The first word
    -- in this name is used in the file extension when no explicit
    -- extension is set.
    targetName :: Text,
    -- | File extension used when generating files for this target.
    targetFileExtension :: Checked f (Maybe Text) FilePath.Ext,
    -- | The format for this target.  Used to compile assets to an
    -- appropriately matching format.
    targetFormat :: Format,
    -- | Controls how some divs may be removed from the AST:
    --
    --   * When parsing from YAML/JSON this is a list of CSS class names.
    --   * When resolved, a set of case-insensitive class names.
    targetRemoveDivs ::
      Checked
        f
        -- List of CSS class names.
        (Maybe (NonEmpty Attrs.CssIdent))
        -- A set of classes to discard.
        (HashSet (CaseInsensitive.CI Attrs.CssIdent)),
    -- | A shell command used to convert Markdown to the desired
    -- format.
    targetCommand :: Checked f Text ((FilePath, FilePath) -> Text)
  }
  deriving stock (Generic)

deriving via (GenericJSON (TargetF Parsed)) instance ToJSON (TargetF Parsed)

deriving via (GenericJSON (TargetF Parsed)) instance FromJSON (TargetF Parsed)

deriving instance Eq (TargetF Parsed)

deriving instance Show (TargetF Parsed)

-- | A fully resolved 'TargetF'.
--
-- @since 0.5.0.0
type Target = TargetF Resolved

-- | Generate a file extension for the given 'Project.Target'.
--
-- @since 0.5.0.0
resolveTargetExtension :: TargetF Parsed -> Maybe FilePath.Ext
resolveTargetExtension Target {..} =
  case targetFileExtension of
    Nothing -> convert targetName
    Just ext -> convert ext
  where
    convert :: Text -> Maybe FilePath.Ext
    convert =
      Text.filter Char.isAlphaNum
        >>> Text.words
        >>> listToMaybe
        >>> fmap (FilePath.Ext . Text.toLower)

-- | Resolve a 'Target' to its final value.
--
-- @since 0.5.0.0
resolve :: Monad m => TargetF Parsed -> ExceptT Error m (TargetF Resolved)
resolve target@Target {..} = do
  ext <-
    case resolveTargetExtension target of
      Nothing ->
        throwError
          ( InvalidTargetFileExtensionError $
              fromMaybe targetName targetFileExtension
          )
      Just ext -> pure ext

  cmd <- hoistEither (resolveCommand targetCommand)

  pure
    Target
      { targetName = targetName,
        targetFileExtension = ext,
        targetFormat = targetFormat,
        targetRemoveDivs = unwantedDivClasses,
        targetCommand = cmd
      }
  where
    resolveCommand :: Text -> Either Error ((FilePath, FilePath) -> Text)
    resolveCommand cmd =
      let boundVars = "io"
          freeVars ast = Placeholders.vars ast \\ boundVars
          toCommand ast (input, output) =
            Placeholders.substitute
              ( fromList
                  [ ('i', toText input),
                    ('o', toText output)
                  ]
              )
              ast
       in case Placeholders.parse cmd of
            Left s -> Left (TargetCommandInvalidFormatError cmd s)
            Right ast
              | null (freeVars ast) ->
                Right (toCommand ast)
              | otherwise ->
                Left (TargetCommandInvalidVarsError cmd boundVars (freeVars ast))

    unwantedDivClasses :: HashSet (CaseInsensitive.CI Attrs.CssIdent)
    unwantedDivClasses =
      case targetRemoveDivs of
        Nothing ->
          mempty
        Just classes ->
          HashSet.fromList $
            toList $
              fmap CaseInsensitive.mk classes
