{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Tests.GetQQ where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON(..), ToJSON(..), withText)
import Data.Aeson.QQ (aesonQQ)
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Aeson.Schema (Object, get, schema)
import Data.Aeson.Schema.TH (mkEnum)
import Data.Aeson.Schema.Utils.Sum (SumType(..))
import Tests.GetQQ.TH
import TestUtils (parseObject)

mkEnum "Greeting" ["HELLO", "GOODBYE"]

newtype Coordinate = Coordinate (Int, Int)
  deriving (Show,Eq)

instance ToJSON Coordinate where
  toJSON (Coordinate (x, y)) = toJSON $ show x ++ "," ++ show y

instance FromJSON Coordinate where
  parseJSON = withText "Coordinate" $ \s ->
    case map (read . Text.unpack) $ Text.splitOn "," s of
      [x, y] -> return $ Coordinate (x, y)
      _ -> fail $ "Bad Coordinate: " ++ Text.unpack s

{- Tests -}

test :: TestTree
test = testGroup "`get` quasiquoter"
  [ testValidExpressions
  , testInvalidExpressions
  ]

testValidExpressions :: TestTree
testValidExpressions = testGroup "Valid get expressions"
  [ testScalarExpressions
  , testBasicExpressions
  , testNullableExpressions
  , testListExpressions
  , testUnionExpressions
  , testPhantomExpressions
  , testNestedExpressions
  ]

testScalarExpressions :: TestTree
testScalarExpressions = testGroup "Scalar expressions"
  [ testProperty "Get Bool key from object" $ \b ->
      let o = $(parseObject "{ foo: Bool }") [aesonQQ| { "foo": #{b} } |]
      in [get| o.foo |] === b

  , testProperty "Get Int key from object" $ \x ->
      let o = $(parseObject "{ foo: Int }") [aesonQQ| { "foo": #{x} } |]
      in [get| o.foo |] === x

  , testProperty "Get Double key from object" $ \x ->
      let o = $(parseObject "{ foo: Double }") [aesonQQ| { "foo": #{x} } |]
      in [get| o.foo |] === x

  , testProperty "Get Text key from object" $ \(UnicodeText s) ->
      let o = $(parseObject "{ foo: Text }") [aesonQQ| { "foo": #{s} } |]
      in [get| o.foo |] === s

  , testProperty "Get Custom key from object" $ \coordinate ->
      let o = $(parseObject "{ foo: Coordinate }") [aesonQQ| { "foo": #{coordinate} } |]
      in [get| o.foo |] === coordinate

  , testProperty "Get Enum key from object" $ \greeting ->
      let o = $(parseObject "{ foo: Greeting }") [aesonQQ| { "foo": #{greeting} } |]
      in [get| o.foo |] === greeting
  ]

testBasicExpressions :: TestTree
testBasicExpressions = testGroup "Basic expressions"
  [ testCase "Can query fields on namespaced object" $
      [get| (Tests.GetQQ.TH.testData).foo |] @?= [get| testData.foo |]

  , testProperty "Can query nested fields" $ \x ->
      let o = $(parseObject "{ foo: { bar: Int } }") [aesonQQ| { "foo": { "bar": #{x} } } |]
      in [get| o.foo.bar |] === x

  , testProperty "Can generate a lambda expression" $ \x ->
      let o = $(parseObject "{ foo: Int }") [aesonQQ| { "foo": #{x} } |]
      in [get| .foo |] o === x

  , testProperty "Can extract a list of elements" $ \x y bar ->
      let o = $(parseObject "{ x: Int, y: Int, foo: { bar: Int } }") [aesonQQ|
            {
              "x": #{x},
              "y": #{y},
              "foo": { "bar": #{bar} }
            }
          |]
      in [get| o.[x, y, x, foo.bar] |] === [x, y, x, bar]

  , testProperty "Can extract a tuple of elements" $ \x b bar ->
      let o = $(parseObject "{ x: Int, b: Bool, foo: { bar: Int } }") [aesonQQ|
            {
              "x": #{x},
              "b": #{b},
              "foo": { "bar": #{bar} }
            }
          |]
      in [get| o.(x,b,x,foo.bar) |] === (x, b, x, bar)
  ]

testNullableExpressions :: TestTree
testNullableExpressions = testGroup "Nullable expressions"
  [ testCase "Get Maybe key from object with value" $ do
      let o = $(parseObject "{ foo: Maybe Bool }") [aesonQQ| { "foo": true } |]
      [get| o.foo |] @?= Just True
      [get| o.foo! |] @?= True

  , testCase "Get Maybe key from object with null value" $
      let o = $(parseObject "{ foo: Maybe Bool }") [aesonQQ| { "foo": null } |]
      in [get| o.foo |] @?= Nothing

  , testCase "Get Maybe key from object without value" $
      let o = $(parseObject "{ foo: Maybe Bool }") [aesonQQ| {} |]
      in [get| o.foo |] @?= Nothing

  , testCase "Unwrapping a nonexistent value fails" $
      let o = $(parseObject "{ foo: Maybe Bool }") [aesonQQ| { "foo": null } |]
      in try @SomeException ([get| o.foo! |] `seq` pure ()) >>= \case
        Right _ -> error "Unexpectedly succeeded"
        Left e -> (head . lines . show) e @?= "Called 'fromJust' on null expression: o.foo"

  , testCase "Can run operations within existing Maybe value" $
      let o = $(parseObject "{ foo: Maybe List { bar: Bool } }") [aesonQQ|
            {
              "foo": [
                { "bar": true },
                { "bar": true },
                { "bar": false }
              ]
            }
          |]
      in [get| o.foo?[].bar |] @?= Just [True, True, False]

  , testCase "Can run operations within nonexisting Maybe value" $
      let o = $(parseObject "{ foo: Maybe List { bar: Bool } }") [aesonQQ| { "foo": null } |]
      in [get| o.foo?[].bar |] @?= Nothing

  , testCase "Can run operations after unwrapping Maybe value" $
      let o = $(parseObject "{ foo: Maybe List { bar: Bool } }") [aesonQQ|
            {
              "foo": [
                { "bar": true },
                { "bar": true },
                { "bar": false }
              ]
            }
          |]
      in [get| o.foo![].bar |] @?= [True, True, False]

  , testCase "Get Try key from object with parsed value" $ do
      let o = $(parseObject "{ foo: Try Bool }") [aesonQQ| { "foo": true } |]
      [get| o.foo |] @?= Just True
      [get| o.foo! |] @?= True

  , testCase "Get Try key from object with invalid value" $
      let o = $(parseObject "{ foo: Try Bool }") [aesonQQ| { "foo": 1 } |]
      in [get| o.foo |] @?= Nothing

  , testCase "Can run operations within parsed Try value" $
      let o = $(parseObject "{ foo: Try List { bar: Bool } }") [aesonQQ|
            {
              "foo": [
                { "bar": true },
                { "bar": true },
                { "bar": false }
              ]
            }
          |]
      in [get| o.foo?[].bar |] @?= Just [True, True, False]

  , testCase "Can run operations within invalid Try value" $
      let o = $(parseObject "{ foo: Try List { bar: Bool } }") [aesonQQ| { "foo": [{ "baz": 1 }] } |]
      in [get| o.foo?[].bar |] @?= Nothing

  , testCase "Can run operations after unwrapping Try value" $
      let o = $(parseObject "{ foo: Try List { bar: Bool } }") [aesonQQ|
            {
              "foo": [
                { "bar": true },
                { "bar": true },
                { "bar": false }
              ]
            }
          |]
      in [get| o.foo![].bar |] @?= [True, True, False]
  ]

testListExpressions :: TestTree
testListExpressions = testGroup "List expressions"
  [ testCase "Get List key from object" $
      let o = $(parseObject "{ foo: List Int }") [aesonQQ| { "foo": [1,2,3] } |]
      in [get| o.foo |] @?= [1,2,3]

  , testProperty "Ending with a `[]` operator is a noop" $ \(xs :: [Int]) ->
      let o = $(parseObject "{ foo: List Int }") [aesonQQ| { "foo": #{xs} } |]
      in [get| o.foo |] === [get| o.foo[] |]

  , testCase "Can run operations within list" $
      let o = $(parseObject "{ foo: List Maybe { bar: Int } }") [aesonQQ|
            {
              "foo": [
                { "bar": 1 },
                { "bar": 2 },
                null,
                { "bar": 3 }
              ]
            }
          |]
      in [get| o.foo[]?.bar |] @?= [Just 1, Just 2, Nothing, Just 3]
  ]

testUnionExpressions :: TestTree
testUnionExpressions = testGroup "Union expressions"
  [ testCase "Get Union key from object" $ do
      let o = $(parseObject "{ foo: Bool | Int }") [aesonQQ| { "foo": 1 } |]
      [get| o.foo |] @?= There (Here 1)
      [get| o.foo@0 |] @?= Nothing
      [get| o.foo@1 |] @?= Just 1

  , testCase "Can run operations after extracting branch" $ do
      let o = $(parseObject "{ foo: { bar: Bool } | { baz: Int } }") [aesonQQ| { "foo": { "bar": true } } |]
      [get| o.foo@0?.bar |] @?= Just True
      [get| o.foo@0!.bar |] @?= True
      [get| o.foo@1?.baz |] @?= Nothing
  ]

testPhantomExpressions :: TestTree
testPhantomExpressions = testGroup "Phantom expressions"
  [ testProperty "Get Phantom key from object" $ \x ->
      let o = $(parseObject "{ [foo]: { bar: Int } }") [aesonQQ| { "bar": #{x} } |]
      in [get| o.foo.bar |] === x
  ]

testNestedExpressions :: TestTree
testNestedExpressions = testGroup "Nested expressions"
  [ testProperty "Extracted object from Object can be queried further" $ \x ->
      let o = $(parseObject "{ foo: { bar: Int } }") [aesonQQ| { "foo": { "bar": #{x} } } |]
          foo = [get| o.foo |]
      in [get| foo.bar |] === x

  , testProperty "Extracted object from Maybe can be queried further" $ \x ->
      let o = $(parseObject "{ foo: Maybe { bar: Int } }") [aesonQQ| { "foo": { "bar": #{x} } } |]
          foo = [get| o.foo! |]
      in [get| foo.bar |] === x

  , testProperty "Extracted object from Try can be queried further" $ \x ->
      let o = $(parseObject "{ foo: Try { bar: Int } }") [aesonQQ| { "foo": { "bar": #{x} } } |]
          foo = [get| o.foo! |]
      in [get| foo.bar |] === x

  , testProperty "Extracted object from List can be queried further" $ \xs ->
      classify (length xs > 1) "non-trivial" $
        let bars = map (\x -> [aesonQQ| { "bar": #{x} } |]) xs
            o = $(parseObject "{ foo: List { bar: Int } }") [aesonQQ| { "foo": #{bars} } |]
            getBar :: Object [schema| { bar: Int } |] -> Int
            getBar foo = [get| foo.bar |]
        in map getBar [get| o.foo |] === xs

  , testProperty "Extracted objects from list of keys can be queried further" $ \fooId barId ->
      let o = $(parseObject "{ foo: { id: Int }, bar: { id: Int } }") [aesonQQ|
            {
              "foo": { "id": #{fooId} },
              "bar": { "id": #{barId} }
            }
          |]
          getId :: Object [schema| { id: Int } |] -> Int
          getId inner = [get| inner.id |]
      in map getId [get| o.[foo, bar] |] === [fooId, barId]

  , testProperty "Extracted objects from tuple of keys can be queried further" $ \a b ->
      let o = $(parseObject "{ foo: { a: Int }, bar: { b: Bool } }") [aesonQQ|
            {
              "foo": { "a": #{a} },
              "bar": { "b": #{b} }
            }
          |]
          (foo, bar) = [get| o.(foo, bar) |]
      in [get| foo.a |] === a .&&. [get| bar.b |] === b

  , testProperty "Extracted objects from union can be queried further" $ \x ->
      let o = $(parseObject "{ foo: { x: Bool } | { x: Int } }") [aesonQQ| { "foo": { "x": #{x} } } |]
      in case [get| o.foo |] of
        There (Here foo) -> [get| foo.x |] === x
        foo -> error $ "Unexpected failure: o.foo = " ++ show foo ++ ", x = " ++ show x
  ]

testInvalidExpressions :: TestTree
testInvalidExpressions = testGroup "Invalid expressions"
  [ testCase "Empty expression" $
      assertError $(tryGetQQ "")
  , testCase "No operators" $
      assertError $(tryGetQQ "")
  , testCase "Operators after tuple of keys" $
      $(getGetQQErr "o.(a,b).foo") @?= str ".(*) operation MUST be last."
  , testCase "Operators after list of keys" $
      $(getGetQQErr "o.[a,b].foo") @?= str ".[*] operation MUST be last."
  ]
  where
    assertError = assertBool "did not error" . isLeft
    -- type hint because of OverloadedStrings
    str = id @String

{- Helpers -}

instance Arbitrary Greeting where
  arbitrary = elements [HELLO, GOODBYE]

instance Arbitrary Coordinate where
  arbitrary = Coordinate <$> arbitrary

newtype UnicodeText = UnicodeText Text
  deriving (Show)

instance Arbitrary UnicodeText where
  arbitrary = UnicodeText . Text.pack . getUnicodeString <$> arbitrary
