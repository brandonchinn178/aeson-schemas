{-|
Module      :  Data.Aeson.Schema
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

This module defines a new way of parsing JSON data by defining type-level schemas and
extracting information using quasiquoters that will check if a given query path is
valid at compile-time.
-}

module Data.Aeson.Schema
  ( -- * Types
    Object
  , SchemaGraph(..)
  , SchemaType
    -- * Functions for loading JSON data with a schema
  , parseSchema
  , decodeWithSchema
    -- * Quasiquoters for extracting JSON data
  , get
  , unwrap
    -- * Magic type classes
  , FromSchema(..)
  ) where

import Data.Aeson.Schema.Internal
import Data.Aeson.Schema.Parse
import Data.Aeson.Schema.QuasiQuoters
