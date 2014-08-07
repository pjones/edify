{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Command to produce heading outlines from Markdown files.
module Outline
       ( Options
       , options
       , dispatch
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import           Options.Applicative
import           System.IO
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Util.HeaderTree

--------------------------------------------------------------------------------
-- | Options for the outline command.
data Options = Options (Maybe FilePath)

--------------------------------------------------------------------------------
-- | Command line parser for the outline command.
options :: Parser Options
options = Options <$> (optional $ argument str (metavar "FILE"))

--------------------------------------------------------------------------------
dispatch :: Options -> IO ()
dispatch (Options file) = print . headers =<< content
  where
    content :: IO String
    content = case file of
      Nothing -> hGetContents stdin
      Just f  -> readFile f

--------------------------------------------------------------------------------
headers :: String -> [HeaderInfo]
headers = onlyHeaders . readMarkdown def
