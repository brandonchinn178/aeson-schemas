{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Schema where

import Data.Aeson.Schema (schema)

type UserSchema = [schema| { "name": Text } |]
type MySchema = [schema| { "extra": Text } |]
