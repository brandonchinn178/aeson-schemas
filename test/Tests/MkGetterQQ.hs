{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.MkGetterQQ where

import Data.Aeson (eitherDecode)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ (r)

import Data.Aeson.Schema (Object, get, mkGetter, schema)
import qualified Data.Aeson.Schema.Internal as Internal

type MySchema = [schema| { users: List { name: Text } } |]

mkGetter "User" "getUsers" ''MySchema ".users[]"

userSchema :: forall schema. (Object schema ~ User) => String
userSchema = Internal.showSchema @schema

test :: TestTree
test = testGroup "`mkGetter` quasiquoter"
  [ testCase "Type synonym is generated" $
      userSchema @?= [r|SchemaObject {"name": Text}|]
  , testCase "Getter function is generated" $
      let users :: [User]
          users = getUsers testData

          getName :: User -> Text
          getName = [get| .name |]

      in map getName users @?= ["Alice", "Bob", "Claire"]
  ]

testData :: Object MySchema
testData = either error id $ eitherDecode $ [r|
  {
    "users": [
      { "name": "Alice" },
      { "name": "Bob" },
      { "name": "Claire" }
    ]
  }
|]
