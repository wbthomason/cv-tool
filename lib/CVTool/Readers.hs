module CVTool.Readers (readJson, readYaml, readToml) where

import Data.Aeson as Ae (eitherDecode', FromJSON(..), toJSON, fromJSON, Result(..))
import Data.ByteString
import Data.Either
import Data.Text (Text)
import Data.Yaml (decodeEither)
import GHC.Generics
import System.Exit
import Text.Pandoc (readNative, Pandoc(..), Meta(..))
import Text.Parsec
import Text.Toml
import Data.Map.Strict


buildPandoc :: (a -> Either String Meta) -> a -> Pandoc
buildPandoc parser rawData = 
  case parser rawData of
        Right cvData -> Pandoc cvData []
        -- TODO: Actually print exception
        Left errMsg  -> Prelude.head $ Data.Either.rights [readNative "hello"]

tomlToJson :: Text -> Result Meta
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
