{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Nested where

import Data.Aeson.Schema

import Util (getMockedResult)

type Schema = [schema|
  {
    "list": List {
      "a": Maybe {
        "b": Int,
      },
      "b": Int,
    },
  }
|]

result :: Object Schema
result = $(getMockedResult "test/nested.json")
