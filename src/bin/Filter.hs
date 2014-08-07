{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
-- | Command to filter a Pandoc JSON stream.
module Filter
       ( Options
       , options
       , dispatch
       ) where

--------------------------------------------------------------------------------
-- Library imports.
import           Options.Applicative
import           Text.Pandoc.JSON

--------------------------------------------------------------------------------
-- | Project imports.
import Text.Edify.Filter (filterPandoc)

--------------------------------------------------------------------------------
-- | No options yet.
data Options = Options

--------------------------------------------------------------------------------
-- | Nothing to parse yet.
options :: Parser Options
options = pure Options

--------------------------------------------------------------------------------
-- | Simple filter.
dispatch :: Options -> IO ()
dispatch _ = toJSONFilter filterPandoc
