{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Object where

import Data.Aeson (Value (..))
import Data.Aeson.QQ (aesonQQ)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson.Schema (Object, schema, toMap)
import qualified Data.Aeson.Schema.Utils.Compat as Compat
import TestUtils (parseValue)
import qualified Tests.Object.Eq
import qualified Tests.Object.FromJSON
import qualified Tests.Object.Show
import qualified Tests.Object.ToJSON

test :: TestTree
test =
  testGroup
    "Object"
    [ Tests.Object.Show.test
    , Tests.Object.Eq.test
    , Tests.Object.FromJSON.test
    , Tests.Object.ToJSON.test
    , testCase "toMap smoketest" $
        let o :: Object [schema| { a: Bool } |]
            o = parseValue [aesonQQ| { "a": true } |]
         in toMap o
              @?= Compat.fromList
                [ ("a", Bool True)
                ]
    ]
