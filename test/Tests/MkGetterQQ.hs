{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.MkGetterQQ where

import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ (r)

import Data.Aeson.Schema (Object, get, mkGetter, schema)
import TestUtils (json, showSchemaResult)

type MySchema = [schema| { users: List { name: Text } } |]

mkGetter "User" "getUsers" ''MySchema ".users[]"

test :: TestTree
test = testGroup "`mkGetter` quasiquoter"
  [ testCase "Type synonym is generated" $
      showSchemaResult @User @?= [r|Object (SchemaObject {"name": Text})|]
  , testCase "Getter function is generated" $
      let users :: [User]
          users = getUsers testData

          getName :: User -> Text
          getName = [get| .name |]

      in map getName users @?= ["Alice", "Bob", "Claire"]
  ]

testData :: Object MySchema
testData = [json|
  {
    "users": [
      { "name": "Alice" },
      { "name": "Bob" },
      { "name": "Claire" }
    ]
  }
|]
