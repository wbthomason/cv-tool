{-# LANGUAGE DeriveGeneric #-}

module CVTool.Readers (readJson, readYaml, readToml) where

import Text.Pandoc (readNative, ReaderOptions(..), Pandoc(..))
import Data.Aeson (eitherDecode', FromJSON(..), toJSON, fromJSON)
import GHC.Generics
import Data.Yaml (decodeEither)
import qualified Text.Toml as TOML
import System.Exit
import Data.ByteString
import Data.Either

data CVData = CVData {
  name :: String
} deriving (Generic, Show)

instance FromJSON CVData

buildPandoc :: (b -> Either String CVData) -> b -> Pandoc
buildPandoc parser rawData = 
  case parser rawData of
        Right cvData -> Prelude.head $ Data.Either.rights [readNative "hello"]
        -- TODO: Actually print exception
        Left errMsg  -> Prelude.head $ Data.Either.rights [readNative "hello"]

readJson = buildPandoc eitherDecode'
readYaml = buildPandoc decodeEither
readToml = buildPandoc $ fromJSON . toJSON . TOML.parseTomlDoc
