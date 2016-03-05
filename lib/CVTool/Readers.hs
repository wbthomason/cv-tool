module CVTool.Readers (readJson, readYaml, readToml) where

import CVTool.Types

import Data.Aeson as Ae (eitherDecode', toJSON, fromJSON, Result(..), FromJSON(..), ToJSON(..), Value(..), Object(..))
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Strict as Map
import Data.Scientific (toRealFloat)
import Data.Text
import qualified Data.Vector as Vec
import Data.Yaml (decodeEither)

import Text.Pandoc
import Text.Parsec
import Text.Toml
import Debug.Trace

metaEncodeValue value =
  case value of
        Object o  -> MetaMap $ makeMap o
        Array a   -> MetaList $ metaEncodeValue <$> Vec.toList a
        String s  -> MetaString $ unpack s
        Number n  -> MetaString $ show . toRealFloat $ n
        Bool b    -> MetaBool b

makeMap cvMap = 
  HashMap.foldlWithKey' (\acc name value -> Map.insert (unpack name) (metaEncodeValue value) acc) Map.empty cvValues
  where cvValues = HashMap.filter (\value -> case value of Ae.Null -> False ; _ -> True) cvMap

buildMeta cvData = 
  Meta { unMeta = makeMap cvData }

toObject cvData =
  cvObject
  where Object cvObject = toJSON cvData

buildPandoc :: (a -> Either String CVData) -> a -> Either String Pandoc
buildPandoc parser inputData = (\meta -> Pandoc meta []) <$> buildMeta <$> toObject <$> parser inputData

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
