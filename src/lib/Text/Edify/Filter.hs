{-

This file is part of the package edify. It is subject to the license
terms in the LICENSE.md file found in the top-level directory of this
distribution and at git://pmade.com/edify/LICENSE.md. No part of the
edify package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE.md file.

-}

--------------------------------------------------------------------------------
module Text.Edify.Filter (filterPandoc) where

--------------------------------------------------------------------------------
-- Library imports.
import Control.Monad (foldM)
import Text.Pandoc

--------------------------------------------------------------------------------
-- Project imports.
import Text.Edify.Filter.Exec (executeBlock)
import Text.Edify.Filter.Insert (insertFile)

--------------------------------------------------------------------------------
filterPandoc :: Pandoc -> IO Pandoc
filterPandoc p = foldM (flip ($)) p [ bottomUpM insertFile
                                    , bottomUpM executeBlock
                                    ]
