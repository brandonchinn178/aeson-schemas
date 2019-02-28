{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AllTypes where

import Data.Aeson (FromJSON(..), withText)
import Data.Aeson.Schema
import qualified Data.Text as Text

import Util (getMockedResult)

{- Greeting enum -}

data Greeting = HELLO | GOODBYE
  deriving (Show,Enum)

instance FromJSON Greeting where
  parseJSON = withText "Greeting" $ \case
    "HELLO" -> pure HELLO
    "GOODBYE" -> pure GOODBYE
    t -> fail $ "Bad Greeting: " ++ Text.unpack t

instance FromSchema ('SchemaCustom "Greeting") where
  type SchemaResult ('SchemaCustom "Greeting") = Greeting

{- Coordinate scalar -}

newtype Coordinate = Coordinate (Int, Int)
  deriving (Show)

instance FromJSON Coordinate where
  parseJSON = withText "Coordinate" $ \s ->
    case map (read . Text.unpack) $ Text.splitOn "," s of
      [x, y] -> return $ Coordinate (x, y)
      _ -> fail $ "Bad Coordinate: " ++ Text.unpack s

instance FromSchema ('SchemaCustom "Coordinate") where
  type SchemaResult ('SchemaCustom "Coordinate") = Coordinate

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
