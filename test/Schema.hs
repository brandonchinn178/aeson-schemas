{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module Schema where

import Data.Aeson (FromJSON)
import Data.Aeson.Schema (schema)

newtype Status = Status Int
  deriving (Show,FromJSON)

type UserSchema = [schema| { "name": Text } |]
type MySchema = [schema| { "extra": Text } |]
