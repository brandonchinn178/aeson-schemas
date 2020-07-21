{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.SchemaQQ.Types where

import Data.Aeson (FromJSON)

import Data.Aeson.Schema (schema)

type UserSchema = [schema| { name: Text } |]
type ExtraSchema = [schema| { extra: Text } |]
type ExtraSchema2 = [schema| { extra: Maybe Text } |]

newtype Status = Status Int
  deriving (Show,FromJSON)
