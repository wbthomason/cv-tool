{-# LANGUAGE DeriveGeneric #-}

module CVTool.Readers (readJson, readYaml, readToml) where

import Data.Aeson as Ae (eitherDecode', toJSON, fromJSON, Result(..), FromJSON(..), ToJSON(..), Value(..), Object(..))
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
import Data.Scientific (toRealFloat)
import qualified Data.Vector as Vec
import qualified Data.HashMap.Strict as HashMap

data CVData = CVData { name :: String } deriving (Generic, Show)

instance FromJSON CVData
instance ToJSON CVData

metaEncodeValue value =
  case value of
        Object o  -> MetaMap $ makeMap o
        Array a   -> MetaList $ metaEncodeValue <$> (Vec.toList a)
        String s  -> MetaString $ unpack s
        Number n  -> MetaString $ show . toRealFloat $ n
        Bool b    -> MetaBool b

makeMap cvObject = 
  HashMap.foldlWithKey' (\acc name value -> Map.insert (unpack name) (metaEncodeValue value) acc) Map.empty cvObject

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
