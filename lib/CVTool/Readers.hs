module CVTool.Readers (readJson, readYaml, readToml) where

import Data.Aeson as Ae (eitherDecode', toJSON, fromJSON, Result(..))
import Data.Yaml (decodeEither)
import Text.Pandoc (Pandoc(..))
import Text.Parsec
import Text.Toml

buildPandoc parser rawData = (\cvData -> Pandoc cvData []) <$> parser rawData

tomlToJson inputData = 
  case parseTomlDoc "InputCVData" inputData of
        Right toml  -> fromJSON . toJSON $ toml
        Left err    -> Ae.Error $ "TOML parse error:" ++ show (errorPos err)

eitherDecodeToml inputData = 
  case tomlToJson inputData of
        Success jsonData     -> Right jsonData
        Ae.Error err         -> Left err

readJson = buildPandoc eitherDecode'
readYaml = buildPandoc decodeEither
readToml = buildPandoc eitherDecodeToml
