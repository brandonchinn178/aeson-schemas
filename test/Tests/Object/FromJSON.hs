{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Object.FromJSON where

import Data.Aeson (FromJSON(..), ToJSON, Value)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (parseEither)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Aeson.Schema (Object, schema)

newtype Coordinate = Coordinate (Int, Int)
  deriving (Show,Eq,ToJSON,FromJSON,Arbitrary)

test :: TestTree
test = testGroup "FromJSON instance"
  [ testProperty "Bool valid" $ \(b :: Bool) ->
      let o :: ParseResult [schema| { foo: Bool } |]
          o = parse [aesonQQ| { "foo": #{b} } |]
      in ioProperty $ assertSuccess o
  , testCase "Bool invalid" $
      let o :: ParseResult [schema| { foo: Bool } |]
          o = parse [aesonQQ| { "foo": 1 } |]
      in assertError o "Error in $: Could not parse path 'foo' with schema `SchemaScalar Bool`: Number 1.0"

  , testProperty "Int valid" $ \(x :: Int) ->
      let o :: ParseResult [schema| { foo: Int } |]
          o = parse [aesonQQ| { "foo": #{x} } |]
      in ioProperty $ assertSuccess o
  , testCase "Int invalid" $
      let o :: ParseResult [schema| { foo: Int } |]
          o = parse [aesonQQ| { "foo": true } |]
      in assertError o "Error in $: Could not parse path 'foo' with schema `SchemaScalar Int`: Bool True"

  , testProperty "Double valid" $ \(x :: Double) ->
      let o :: ParseResult [schema| { foo: Double } |]
          o = parse [aesonQQ| { "foo": #{x} } |]
      in ioProperty $ assertSuccess o
  , testCase "Double invalid" $
      let o :: ParseResult [schema| { foo: Double } |]
          o = parse [aesonQQ| { "foo": true } |]
      in assertError o "Error in $: Could not parse path 'foo' with schema `SchemaScalar Double`: Bool True"

  , testProperty "Text valid" $ \(x :: String) ->
      let o :: ParseResult [schema| { foo: Text } |]
          o = parse [aesonQQ| { "foo": #{x} } |]
      in ioProperty $ assertSuccess o
  , testCase "Text invalid" $
      let o :: ParseResult [schema| { foo: Text } |]
          o = parse [aesonQQ| { "foo": true } |]
      in assertError o "Error in $: Could not parse path 'foo' with schema `SchemaScalar Text`: Bool True"

  , testProperty "Custom valid" $ \(x :: Coordinate) ->
      let o :: ParseResult [schema| { foo: Coordinate } |]
          o = parse [aesonQQ| { "foo": #{x} } |]
      in ioProperty $ assertSuccess o
  , testCase "Custom invalid" $
      let o :: ParseResult [schema| { foo: Coordinate } |]
          o = parse [aesonQQ| { "foo": true } |]
      in assertError o "Error in $: Could not parse path 'foo' with schema `SchemaScalar Coordinate`: Bool True"

  , testProperty "Maybe valid" $ \(x :: Maybe Int) ->
      let o :: ParseResult [schema| { foo: Maybe Int } |]
          o = parse [aesonQQ| { "foo": #{x} } |]
      in ioProperty $ assertSuccess o
  , testCase "Maybe invalid" $
      let o :: ParseResult [schema| { foo: Maybe Int } |]
          o = parse [aesonQQ| { "foo": true } |]
      in assertError o "Error in $: Could not parse path 'foo' with schema `SchemaScalar Int`: Bool True"

  , testProperty "Try valid with valid parse" $ \(x :: Int) ->
      let o :: ParseResult [schema| { foo: Try Int } |]
          o = parse [aesonQQ| { "foo": #{x} } |]
      in ioProperty $ assertSuccess o
  , testProperty "Try valid with invalid parse" $ \(s :: String) ->
      let o :: ParseResult [schema| { foo: Try Int } |]
          o = parse [aesonQQ| { "foo": #{s} } |]
      in ioProperty $ assertSuccess o

  , testProperty "List valid" $ \(xs :: [Int]) ->
      let o :: ParseResult [schema| { foo: List Int } |]
          o = parse [aesonQQ| { "foo": #{xs} } |]
      in ioProperty $ assertSuccess o
  , testCase "List invalid" $
      let o :: ParseResult [schema| { foo: List Int } |]
          o = parse [aesonQQ| { "foo": true } |]
      in assertError o "Error in $: Could not parse path 'foo' with schema `SchemaList Int`: Bool True"
  , testCase "List invalid inner" $
      let o :: ParseResult [schema| { foo: List Int } |]
          o = parse [aesonQQ| { "foo": [true] } |]
      in assertError o "Error in $: Could not parse path 'foo' with schema `SchemaScalar Int`: Bool True"

  , testCase "Object invalid" $
      let o :: ParseResult [schema| { foo: Int } |]
          o = parse [aesonQQ| 1 |]
      in assertError o "Error in $: Could not parse schema `SchemaObject {\"foo\": Int}`: Number 1.0"
  , testCase "Object invalid in later keys" $
      let o :: ParseResult [schema| { foo: Int, bar: Int } |]
          o = parse [aesonQQ| { foo: 1, bar: true } |]
      in assertError o "Error in $: Could not parse path 'bar' with schema `SchemaScalar Int`: Bool True"

  , testProperty "Nested object valid" $ \(x :: Int) ->
      let o :: ParseResult [schema| { foo: { bar: Int } } |]
          o = parse [aesonQQ| { "foo": { "bar": #{x} } } |]
      in ioProperty $ assertSuccess o
  , testCase "Nested object invalid" $
      let o :: ParseResult [schema| { foo: { bar: Int } } |]
          o = parse [aesonQQ| { "foo": true } |]
      in assertError o "Error in $: Could not parse path 'foo' with schema `SchemaObject {\"bar\": Int}`: Bool True"
  , testCase "Nested object invalid inner" $
      let o :: ParseResult [schema| { foo: { bar: Int } } |]
          o = parse [aesonQQ| { "foo": { "bar": true } } |]
      in assertError o "Error in $: Could not parse path 'foo.bar' with schema `SchemaScalar Int`: Bool True"

  , testProperty "Union object valid" $ \(x :: Int) ->
      let o :: ParseResult [schema| { foo: Int | Text } |]
          o = parse [aesonQQ| { "foo": #{x} } |]
      in ioProperty $ assertSuccess o
  , testCase "Union object invalid" $
      let o :: ParseResult [schema| { foo: Int | Text } |]
          o = parse [aesonQQ| { "foo": true } |]
      in assertError o "Error in $: Could not parse path 'foo' with schema `SchemaUnion ( Int | Text )`: Bool True"

  , testProperty "Phantom key valid" $ \(x :: Int) ->
      let o :: ParseResult [schema| { [foo]: { bar: Int } } |]
          o = parse [aesonQQ| { "bar": #{x} } |]
      in ioProperty $ assertSuccess o

  , testCase "Decode failure messages are truncated" $
      let o :: ParseResult [schema| { foo: Int } |]
          o = parse [aesonQQ|
            {
              "foo": [
                { "bar": 1, "baz": "a" },
                { "bar": 2, "baz": "b" },
                { "bar": 3, "baz": "c" },
                { "bar": 4, "baz": "d" }
              ]
            }
          |]
      in assertError o $ concat
        [ "Error in $: Could not parse path 'foo' with schema `SchemaScalar Int`: Array ["
        , "Object (fromList [(\"baz\",String \"a\"),(\"bar\",Number 1.0)]),"
        , "Object (fromList [(\"baz\",String \"b\"),(\"bar\",Number 2.0)]),"
        , "Object (fromList [(\"baz\",String \"c\"),(\"bar\",Number 3.0)]),"
        , "Object (fromList [(..."
        ]
  ]

{- Helpers -}

type ParseResult schema = Either String (Object schema)

parse :: FromJSON a => Value -> Either String a
parse = parseEither parseJSON

assertSuccess :: ParseResult schema -> Assertion
assertSuccess = \case
  Right _ -> return ()
  Left e -> fail $ "Unexpected failure: " ++ e

assertError :: Show (Object schema) => ParseResult schema -> String -> Assertion
assertError result msg =
  case result of
    Right o -> assertFailure $ "Unexpectedly parsed: " ++ show o
    Left e -> e @?= msg
