{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Object where

import Data.Aeson (Value(..))
import Data.Aeson.QQ (aesonQQ)
import qualified Data.HashMap.Lazy as HashMap
import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson.Schema (Object, schema, toMap)
import qualified Tests.Object.FromJSON
import qualified Tests.Object.Show
import TestUtils (parseValue)

test :: TestTree
test = testGroup "Object"
  [ Tests.Object.Show.test
  , Tests.Object.FromJSON.test

  , testCase "toMap smoketest" $
      let o :: Object [schema| { a: Bool } |]
          o = parseValue [aesonQQ| { "a": true } |]
      in toMap o @?= HashMap.fromList
        [ ("a", Bool True)
        ]
  ]
