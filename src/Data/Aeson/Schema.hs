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
  , attachSchema
  , decodeWithSchema
    -- * Quasiquoters for extracting JSON data
  , get
  , unwrap
  ) where

import Data.Aeson.Schema.Internal
