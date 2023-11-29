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
module Main
  ( main,
  )
where

import Data.Aeson (FromJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import Data.Binary.Instances.Text
import Data.Binary.Instances.UnorderedContainers
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

newtype EntityValue = EV Text
  deriving (Show)

instance FromJSON EntityValue where
  parseJSON = Aeson.withObject "Entity" $ \v ->
    EV <$> v Aeson..: "characters"

type Entities = HashMap Text EntityValue

main :: IO ()
main = do
  input <- LByteString.getContents
  case Aeson.decode input of
    Nothing -> die "failed to decode entities JSON from stdin"
    Just e -> go e
  where
    go :: Entities -> IO ()
    go = transformMap >>> printHaskell

    transformMap :: Entities -> HashMap Text Text
    transformMap =
      HashMap.foldrWithKey
        ( \k (EV v) m ->
            if isModernEntity k
              then HashMap.insert (prepareKey k) v m
              else m
        )
        mempty

    isModernEntity :: Text -> Bool
    isModernEntity key =
      not (Text.null key)
        && Text.head key == '&'
        && Text.last key == ';'

    prepareKey :: Text -> Text
    prepareKey = Text.drop 1 >>> Text.dropEnd 1

    printHaskell :: HashMap Text Text -> IO ()
    printHaskell table = do
      putTextLn "entities :: HashMap Text Text"
      putText "entities = let es = Binary.decode "
      print (Binary.encode table)
      putTextLn "           in es"
