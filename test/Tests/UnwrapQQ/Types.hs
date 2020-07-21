{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.UnwrapQQ.Types where

import Data.Aeson.Schema (schema)

type ListSchema = [schema| { ids: List Int } |]
type MaybeSchema = [schema| { class: Maybe Text } |]
type SumSchema = [schema| { verbosity: Int | Bool } |]

type MySchema = [schema|
  {
    users: List {
      name: Text,
    },
  }
|]
