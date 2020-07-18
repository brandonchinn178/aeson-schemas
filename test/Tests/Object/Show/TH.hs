{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Object.Show.TH where

import Data.Aeson.Schema (schema)

type UserSchema = [schema| { name: Text } |]
