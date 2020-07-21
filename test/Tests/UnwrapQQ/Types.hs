{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.UnwrapQQ.Types where

import Data.Aeson.Schema (Object, schema)

type ListSchema = [schema| { ids: List Int } |]
type MaybeSchema = [schema| { class: Maybe Text } |]
type SumSchema = [schema| { verbosity: Int | Bool } |]
type ABCSchema = [schema|
  {
    a: Int,
    b: Int,
    c: Bool,
  }
|]

type MySchema = [schema|
  {
    users: List {
      name: Text,
    },
  }
|]

type MySchemaResult = Object MySchema
