{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Bootstrap where

import Data.Aeson (parseJSON)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (parseEither)

import Data.Aeson.Schema

type MySchema = [schema|
  {
    users: List Maybe {
      name: Text,
    },
  }
|]

result :: Object MySchema
result = either error id $ parseEither parseJSON [aesonQQ|
  {
    "users": [
      { "name": "Alice" },
      { "name": "Bob" },
      null,
      { "name": "Claire" }
    ]
  }
|]
