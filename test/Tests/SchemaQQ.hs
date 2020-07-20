{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Tests.SchemaQQ where

import Data.Aeson (FromJSON)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ (r)

import Data.Aeson.Schema (Schema, schema)
import qualified Data.Aeson.Schema.Internal as Internal
import Tests.SchemaQQ.TH

test :: TestTree
test = testGroup "`schema` quasiquoter"
  [ testValidSchemas
  , testInvalidSchemas
  ]

testValidSchemas :: TestTree
testValidSchemas = testGroup "Valid schemas"
  [ testCase "Object with Bool field" $
      assertSchemaMatches
        @[schema| { a: Bool } |]
        [r| SchemaObject {"a": Bool} |]

  , testCase "Object with Int field" $
      assertSchemaMatches
        @[schema| { a: Int } |]
        [r| SchemaObject {"a": Int} |]

  , testCase "Object with Double field" $
      assertSchemaMatches
        @[schema| { foo123: Double } |]
        [r| SchemaObject {"foo123": Double} |]

  , testCase "Object with Text field" $
      assertSchemaMatches
        @[schema| { some_text: Text } |]
        [r| SchemaObject {"some_text": Text} |]

  , testCase "Object with a field with a custom type" $
      assertSchemaMatches
        @[schema| { status: Status } |]
        [r| SchemaObject {"status": Status} |]

  , testCase "Object with a field with a Maybe type" $
      assertSchemaMatches
        @[schema| { a: Maybe Bool } |]
        [r| SchemaObject {"a": Maybe Bool} |]

  , testCase "Object with a field with a Try type" $
      assertSchemaMatches
        @[schema| { a: Try Bool } |]
        [r| SchemaObject {"a": Try Bool} |]

  , testCase "Object with a nested object" $
      assertSchemaMatches
        @[schema| { a: { b: Int } } |]
        [r| SchemaObject {"a": {"b": Int}} |]

  , testCase "Object with a nullable nested object" $
      assertSchemaMatches
        @[schema| { a: Maybe { b: Int } } |]
        [r| SchemaObject {"a": Maybe {"b": Int}} |]

  , testCase "Object with a list of nested objects" $
      assertSchemaMatches
        @[schema| { a: List { b: Int } } |]
        [r| SchemaObject {"a": List {"b": Int}} |]

  , testCase "Object with an imported schema" $
      assertSchemaMatches
        @[schema| { user: #UserSchema } |]
        [r| SchemaObject {"user": {"name": Text}} |]

  , testCase "Object with an qualified imported schema" $
      assertSchemaMatches
        @[schema| { user: #(Tests.SchemaQQ.TH.UserSchema) } |]
        [r| SchemaObject {"user": {"name": Text}} |]

  , testCase "Object with an extended schema" $
      assertSchemaMatches
        @[schema| { a: Int, #ExtraSchema } |]
        [r| SchemaObject {"a": Int, "extra": Text} |]

  , testCase "Object with a qualified extended schema" $
      assertSchemaMatches
        @[schema| { a: Int, #(Tests.SchemaQQ.TH.ExtraSchema) } |]
        [r| SchemaObject {"a": Int, "extra": Text} |]

  , testCase "Object with an extended schema with a shadowed key" $
      assertSchemaMatches
        @[schema| { extra: Bool, #ExtraSchema } |]
        [r| SchemaObject {"extra": Bool} |]

  , testCase "Object with a qualified extended schema with a shadowed key" $
      assertSchemaMatches
        @[schema| { extra: Bool, #(Tests.SchemaQQ.TH.ExtraSchema) } |]
        [r| SchemaObject {"extra": Bool} |]

  , testCase "Object with a union field" $
      assertSchemaMatches
        @[schema| { a: List Int | Text } |]
        [r| SchemaObject {"a": ( List Int | Text )} |]

  , testCase "Object with a union field with a group" $
      assertSchemaMatches
        @[schema| { a: List (Int | Text) } |]
        [r| SchemaObject {"a": List ( Int | Text )} |]

  , testCase "Object with a phantom key" $
      assertSchemaMatches
        @[schema| { [a]: { b: Int } } |]
        [r| SchemaObject {"a": {"b": Int}} |]
  ]

testInvalidSchemas :: TestTree
testInvalidSchemas = testGroup "Invalid schemas"
  [ testCase "Object with a duplicate key" $
      [schemaErr| { a: Int, a: Bool } |] @?= "Key 'a' specified multiple times"

  , testCase "Object with a duplicate phantom key" $
      [schemaErr| { a: Int, [a]: { b: Bool } } |] @?= "Key 'a' specified multiple times"

  , testCase "Object with a duplicate key from extending" $
      [schemaErr| { #ExtraSchema, #ExtraSchema2 } |] @?= "Key 'extra' declared in multiple imported schemas"

  , testCase "Quasiquoter defining a non-object" $
      [schemaErr| List { a: Int } |] @?= "`schema` definition must be an object"

  , testCase "Object with a field with an unknown type" $
      [schemaErr| { a: HelloWorld } |] @?= "Unknown type: HelloWorld"

  , testCase "Object extending a non-schema" $
      [schemaErr| { #Int } |] @?= "'GHC.Types.Int' is not a Schema"

  , testCase "Object with a phantom key for a non-object" $
      [schemaErr| { [a]: Int } |] @?= "Invalid schema for 'a': SchemaInt"
  ]

{- Helpers -}

newtype Status = Status Int
  deriving (Show,FromJSON)

assertSchemaMatches :: forall (schema :: Schema). Typeable (Internal.ToSchemaObject schema) => String -> Assertion
assertSchemaMatches = (schemaStr @?=) . Text.unpack . Text.strip . Text.pack
  where
    schemaStr = Internal.showSchemaType @(Internal.ToSchemaObject schema)
