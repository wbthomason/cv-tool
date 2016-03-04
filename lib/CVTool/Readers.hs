{-# LANGUAGE DeriveGeneric #-}

module CVTool.Readers (readJson, readYaml, readToml) where

import Data.Aeson as Ae (eitherDecode', toJSON, fromJSON, Result(..), FromJSON(..), Value(..), Object(..))
import Data.Yaml (decodeEither)
import Text.Pandoc
import Text.Parsec
import Text.Toml
import Debug.Trace
import Data.Text.Encoding
import Data.Map.Strict as Map
import GHC.Generics
{- This is kinda hacky and bad. Should do this with GHC.Generics, but so much
 - boilerplate -}
import Data.Data
import Data.Text

data CVData = CVData { name :: String } deriving (Generic, Show)

instance FromJSON CVData

metaEncodeValue value =
  case value of
        Map k v
        String s
        Bool t


makeMap cvData = 
  Map.foldl (\acc (name, value) -> insert (unpack name) (metaEncodeValue value) acc) Map.empty cvMap
  where Object cvMap = toJSON cvData

buildMeta cvData = 
  (\cv -> Meta { unMeta = makeMap cv }) <$> cvData


buildPandoc :: (a -> Either String CVData) -> a -> Either String Pandoc
buildPandoc parser inputData = (\meta -> Pandoc meta []) <$> (buildMeta . parser) inputData

tomlToJson :: Text -> Result CVData
tomlToJson inputData = 
  case parseTomlDoc "InputCVData" inputData of
        Right toml  -> fromJSON . toJSON $ toml
        Left err    -> Ae.Error $ "TOML parse error: " ++ show (errorPos err)

eitherDecodeToml inputData =
  case tomlToJson inputData of
        Success jsonData     -> Right jsonData
        Ae.Error err         -> Left $ "TOML conversion error: '" ++ err ++ "'"

readJson = buildPandoc eitherDecode'
readYaml = buildPandoc decodeEither
readToml = buildPandoc eitherDecodeToml
