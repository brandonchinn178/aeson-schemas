{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Get where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson.Schema (Object, schema)
import Data.Aeson.Schema.Get
import TestUtils (json)

{- Tests -}

test :: TestTree
test = testGroup "`get` expressions without quasiquoters"
  [ testCase "Can extract keys" $ do
      let result :: Object [schema| { users: List { name: Text } } |]
          result = [json|
            {
              "users": [
                { "name": "Alice" },
                { "name": "Bob" },
                { "name": "Claire" }
              ]
            }
          |]

          users = get (key #users) result

      map (get (key #name)) users @?= ["Alice", "Bob", "Claire"]
  , testCase "Can extract keys within lists" $ do
      let result :: Object [schema| { users: List { name: Text } } |]
          result = [json|
            {
              "users": [
                { "name": "Alice" },
                { "name": "Bob" },
                { "name": "Claire" }
              ]
            }
          |]

      get (key #name . intoList . key #users) result @?= ["Alice", "Bob", "Claire"]
  , testCase "Can extract keys within nullable objects" $ do
      let result :: Object [schema| { userExists: Maybe { name: Text }, userMissing: Maybe { name: Text } } |]
          result = [json|
            {
              "userExists": { "name": "Alice" },
              "userMissing": null
            }
          |]

      get (key #name . intoMaybe . key #userExists) result @?= Just "Alice"
      get (key #name . bang . key #userExists) result @?= "Alice"
      get (key #name . intoMaybe . key #userMissing) result @?= Nothing
  ]
