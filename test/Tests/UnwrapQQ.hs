{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.UnwrapQQ where

import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ (r)

import Data.Aeson.Schema (Object, get, unwrap)
import Tests.UnwrapQQ.TH
import TestUtils (json)

test :: TestTree
test = testGroup "`unwrap` quasiquoter"
  [ testValidUnwrapDefs
  , testInvalidUnwrapDefs
  ]

type User = [unwrap| MySchema.users[] |]

testValidUnwrapDefs :: TestTree
testValidUnwrapDefs = testGroup "Valid unwrap definitions"
  [ testCase "Can unwrap a list" $ do
      [unwrapRep| ListSchema.ids |] @?= "[Int]"
      [unwrapRep| ListSchema.ids[] |] @?= "Int"

  , testCase "Can unwrap a list of keys" $
      [unwrapRep| ABCSchema.[a, b] |] @?= "[Int]"

  , testCase "Can unwrap a tuple of keys" $
      [unwrapRep| ABCSchema.(a, b, c) |] @?= "(Int,Int,Bool)"

  , testCase "Can unwrap a maybe" $ do
      [unwrapRep| MaybeSchema.class |] @?= "Maybe Text"
      [unwrapRep| MaybeSchema.class! |] @?= "Text"
      [unwrapRep| MaybeSchema.class? |] @?= "Text"

  , testCase "Can unwrap from an Object" $
      [unwrapRep| MySchemaResult.users[].name |] @?= "Text"

  , testCase "Can unwrap a sum type" $ do
      [unwrapRep| SumSchema.verbosity@0 |] @?= "Int"
      [unwrapRep| SumSchema.verbosity@1 |] @?= "Bool"

  , testCase "Can use unwrapped type" $ do
      let result :: Object MySchema
          result = [json|
            {
              "users": [
                { "name": "Alice" },
                { "name": "Bob" },
                { "name": "Claire" }
              ]
            }
          |]

          users :: [User]
          users = [get| result.users |]

          getName :: User -> String
          getName = Text.unpack . [get| .name |]

      map getName users @?= ["Alice", "Bob", "Claire"]
  ]

testInvalidUnwrapDefs :: TestTree
testInvalidUnwrapDefs = testGroup "Invalid unwrap definitions"
  [ testCase "Unwrap unknown schema" $
      [unwrapErr| FooSchema.asdf |] @?= "Unknown schema: FooSchema"

  , testCase "Unwrap non-schema" $
      let msg = [unwrapErr| NotASchema.foo |]
          isPrefixOf a b = Text.isPrefixOf (Text.pack a) (Text.pack b)
      in assertBool ("Error message does not match: " ++ msg) $
        "Unknown reified schema: " `isPrefixOf` msg

  , testCase "Unwrap key on non-object" $
      [unwrapErr| ListSchema.ids.foo |] @?= "Cannot get key 'foo' in schema: SchemaList Int"

  , testCase "Unwrap maybe on non-maybe" $ do
      [unwrapErr| ListSchema.ids! |] @?= "Cannot use `!` operator on schema: SchemaList Int"
      [unwrapErr| ListSchema.ids? |] @?= "Cannot use `?` operator on schema: SchemaList Int"

  , testCase "Unwrap list on non-list" $
      [unwrapErr| MaybeSchema.class[] |] @?= "Cannot use `[]` operator on schema: SchemaMaybe Text"

  , testCase "Unwrap nonexistent key" $
      [unwrapErr| ListSchema.foo |] @?= [r|Key 'foo' does not exist in schema: SchemaObject {"ids": List Int}|]

  , testCase "Unwrap list of keys with different types" $
      [unwrapErr| ABCSchema.[a,b,c] |] @?= [r|List contains different types with schema: SchemaObject {"a": Int, "b": Int, "c": Bool}|]

  , testCase "Unwrap list of keys on non-object schema" $
      [unwrapErr| ListSchema.ids.[a,b] |] @?= "Cannot get keys in schema: SchemaList Int"

  , testCase "Unwrap tuple of keys on non-object schema" $
      [unwrapErr| ListSchema.ids.(a,b) |] @?= "Cannot get keys in schema: SchemaList Int"

  , testCase "Unwrap branch on non-branch" $
      [unwrapErr| MaybeSchema.class@0 |] @?= "Cannot use `@` operator on schema: SchemaMaybe Text"

  , testCase "Unwrap out of bounds branch" $
      [unwrapErr| SumSchema.verbosity@10 |] @?= "Branch out of bounds for schema: SchemaUnion ( Int | Bool )"
  ]
