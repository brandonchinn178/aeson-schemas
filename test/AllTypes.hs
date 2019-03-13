{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module AllTypes where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema
import Data.Aeson.Schema.TH (mkEnum)
import qualified Data.Text as Text

import Util (getMockedResult)

{- Greeting enum -}

mkEnum "Greeting" ["HELLO", "GOODBYE"]

{- Coordinate scalar -}

newtype Coordinate = Coordinate (Int, Int)
  deriving (Show)

instance FromJSON Coordinate where
  parseJSON = withText "Coordinate" $ \s ->
    case map (read . Text.unpack) $ Text.splitOn "," s of
      [x, y] -> return $ Coordinate (x, y)
      _ -> fail $ "Bad Coordinate: " ++ Text.unpack s

{- AllTypes result -}

type Schema = [schema|
  {
    "bool": Bool,
    "int": Int,
    "int2": Int,
    "double": Double,
    "text": Text,
    "scalar": Coordinate,
    "enum": Greeting,
    "maybeObject": Maybe {
      "text": Text,
    },
    "maybeObjectNull": Maybe {
      "text": Text,
    },
    "maybeList": Maybe List {
      "text": Text,
    },
    "maybeListNull": Maybe List {
      "text": Text,
    },
    "list": List {
      "type": Text,
      "maybeBool": Maybe Bool,
      "maybeInt": Maybe Int,
      "maybeNull": Maybe Bool,
    },
    "nonexistent": Maybe Text,
  }
|]

result :: Object Schema
result = $(getMockedResult "test/all_types.json")
