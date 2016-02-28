{-# LANGUAGE DeriveGeneric #-}

module CVTool.Readers (readJson, readYaml, readToml) where

import Data.Aeson as Ae (eitherDecode', toJSON, fromJSON, Result(..), FromJSON(..))
import Data.Yaml (decodeEither)
import Text.Pandoc
import Text.Parsec
import Text.Toml
import Debug.Trace
import Data.Text.Encoding
import Data.Map.Strict as Map
import GHC.Generics
import Data.Text

data CVData = CVData { name :: String } deriving (Generic, Show)

instance FromJSON CVData

buildMeta cvData = Meta { unMeta = Map.fromList [("name", MetaString "Hello")] }

buildPandoc :: (a -> Either String CVData) -> b -> Either String Pandoc
buildPandoc parser rawData = (\cvData -> Pandoc cvData []) <$> buildMeta . parser $ rawData

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
