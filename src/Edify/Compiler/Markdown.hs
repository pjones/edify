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
module Edify.Compiler.Markdown
  ( Markdown (..),
    compile,
  )
where

import Control.Lens ((.~), (?~), (^.), _1)
import qualified Data.Attoparsec.Text.Lazy as Atto
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Edify.Compiler.Lang (Compiler)
import qualified Edify.Compiler.Lang as C
import qualified Edify.Format as Format
import Edify.Input (Input)
import qualified Edify.Input as Input
import qualified Edify.Markdown.AST as AST
import Edify.Markdown.Attributes (Attributes)
import qualified Edify.Markdown.Attributes as Attrs
import Edify.Markdown.Fence (Rewrite (..))
import qualified Edify.Markdown.Fence as Fence
import qualified Edify.Markdown.Include as Include

-- | A compiled Markdown document.
--
-- @since 0.5.0.0
data Markdown = Markdown
  { -- | The initial input from which this document was generated.
    markdownInput :: Input,
    -- | The final, rewritten Markdown syntax tree.
    markdownAST :: AST.AST
  }

-- | Compile the given 'Input' as Markdown.
--
-- @since 0.5.0.0
compile :: Input -> Compiler Markdown
compile input =
  C.readInput input $ \content ->
    parse content
      >>= AST.blocks process
      <&> Markdown input
  where
    parse :: LText -> Compiler AST.AST
    parse content =
      case Atto.parse (AST.markdownP <* Atto.endOfInput) content of
        Atto.Fail _ context msg ->
          C.abort (C.ParseError input context msg)
        Atto.Done leftover ast
          | LText.null leftover ->
            pure ast
          | otherwise ->
            C.abort
              ( C.ParseError
                  input
                  []
                  ( "impossible: text after EOF: "
                      <> toString leftover
                  )
              )

    process :: AST.Block -> Compiler [AST.Block]
    process = \case
      AST.IncludeBlock Include.Include {..} -> do
        Markdown {..} <- case includeToken of
          Nothing -> compile (Input.FromFile includeFile)
          Just token -> do
            let format = Format.fromFileExtension includeFile
            C.readInput (Input.FromFile includeFile) $ \content ->
              Format.narrow format token content
                & either
                  (C.abort . C.FormatError input)
                  (compile . Input.FromText)
        pure
          ( AST.unAST markdownAST
              <> one (AST.BlankLine includeEndOfLine)
          )
      other -> rewrite other

    rewrite :: AST.Block -> Compiler [AST.Block]
    rewrite block = do
      ts <- C.tabstop
      classes <- C.unwantedDivClasses

      let rewrites =
            [ rewriteInsert,
              rewriteExec,
              pure . Fence.discardMatchingDivs classes . fst
            ]
          go r =
            traverse ($ r) rewrites
              <&> mconcat

      AST.fencesRewrite ts go block
        >>= either
          (C.abort . C.DivRewriteError input)
          (AST.urls rewriteLink)

-- | Rewrite link URLs that point to local assets.
rewriteLink :: Text -> Compiler Text
rewriteLink url
  | "://" `Text.isInfixOf` url = pure url
  | otherwise = C.asset (toString url) <&> toText

-- | Generate a 'Rewrite' from a request to insert a (possibly
-- narrowed) source file.
--
-- @since 0.5.0.0
rewriteInsert ::
  -- | Attributes that were sourced from some external source.
  (Attributes, Text) ->
  -- | An error or a rewrite request.
  Compiler Rewrite
rewriteInsert src
  | Just file <- src ^. _1 . Attrs.at "insert" =
    insert (toString file)
  | otherwise =
    pure (Rewrite Nothing Nothing)
  where
    insert :: FilePath -> Compiler Rewrite
    insert file = do
      let input = Input.FromFile file
      C.readInput input $ \text -> do
        body <- case src ^. _1 . Attrs.at "token" of
          Nothing ->
            pure text
          Just token ->
            let format = Format.fromFileExtension file
             in Format.narrow format (Format.Token token) text
                  & either (C.abort . C.FormatError input) pure
        pure
          Rewrite
            { rewriteAttrs =
                Just
                  ( src ^. _1
                      & Attrs.at "token" .~ Nothing
                      & Attrs.at "insert" .~ Nothing
                      & Attrs.at "inserted" ?~ toText file
                  ),
              rewriteBody = Just (toStrict body)
            }

-- | Generate a Markdown fence rewrite request if the given attributes
-- are requesting the execution of a shell command.
--
-- NOTE: For security conscious readers: This code doesn't actually
-- execute anything.  It uses the 'C.exec' DSL feature which handles the
-- security risks around executing arbitrary shell commands.
--
-- @since 0.5.0.0
rewriteExec ::
  -- | Sourced attributes and standard input if execution is requested.
  (Attributes, Text) ->
  -- | Error or rewrite request.
  Compiler Rewrite
rewriteExec src
  | Just command <- src ^. _1 . Attrs.at "exec" = go command
  | otherwise = pure (Rewrite Nothing Nothing)
  where
    go :: Text -> Compiler Rewrite
    go command = do
      output <- C.exec (src & _1 .~ command)
      pure
        Rewrite
          { rewriteAttrs =
              Just
                ( src ^. _1
                    & Attrs.at "exec" .~ Nothing
                    & Attrs.at "execed" ?~ command
                ),
            rewriteBody = Just output
          }
