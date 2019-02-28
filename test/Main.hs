{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Aeson.Schema (Object, get, unwrap)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

import qualified AllTypes
import qualified Nested

allTypes :: Object AllTypes.Schema
allTypes = AllTypes.result

main :: IO ()
main = defaultMain $ testGroup "aeson-schemas"
  [ testGetterExp
  , testFromObjectAllTypes
  , testFromObjectNested
  , testFromObjectNamespaced
  , testUnwrapSchema
  ]

goldens' :: String -> IO String -> TestTree
goldens' name = goldenVsString name fp . fmap ByteString.pack
  where
    fp = "test/goldens/" ++ name ++ ".golden"

goldens :: Show s => String -> s -> TestTree
goldens name = goldens' name . pure . show

testGetterExp :: TestTree
testGetterExp = testGroup "Test getter expressions"
  [ goldens "bool"                     [get| allTypes.bool                  |]
  , goldens "lambda_bool"             ([get| .bool |] allTypes)
  , goldens "int"                      [get| allTypes.int                   |]
  , goldens "int_int2"                 [get| allTypes.[int,int2]            |]
  , goldens "double"                   [get| allTypes.double                |]
  , goldens "bool_int_double"          [get| allTypes.(bool,int,double)     |]
  , goldens "text"                     [get| allTypes.text                  |]
  , goldens "scalar"                   [get| allTypes.scalar                |]
  , goldens "enum"                     [get| allTypes.enum                  |]
  , goldens "maybeObj"                 [get| allTypes.maybeObject           |]
  , goldens "maybeObj_bang"            [get| allTypes.maybeObject!          |]
  , goldens "maybeObj_text"            [get| allTypes.maybeObject?.text     |]
  , goldens "maybeObj_bang_text"       [get| allTypes.maybeObject!.text     |]
  , goldens "maybeObjNull"             [get| allTypes.maybeObjectNull       |]
  , goldens "maybeObjNull_text"        [get| allTypes.maybeObjectNull?.text |]
  , goldens "maybeList"                [get| allTypes.maybeList             |]
  , goldens "maybeList_bang"           [get| allTypes.maybeList!            |]
  , goldens "maybeList_bang_list"      [get| allTypes.maybeList![]          |]
  , goldens "maybeList_bang_list_text" [get| allTypes.maybeList![].text     |]
  , goldens "maybeList_list"           [get| allTypes.maybeList?[]          |]
  , goldens "maybeList_list_text"      [get| allTypes.maybeList?[].text     |]
  , goldens "maybeListNull"            [get| allTypes.maybeListNull         |]
  , goldens "maybeListNull_list"       [get| allTypes.maybeListNull?[]      |]
  , goldens "maybeListNull_list_text"  [get| allTypes.maybeListNull?[].text |]
  , goldens "list"                     [get| allTypes.list                  |]
  , goldens "list_explicit"            [get| allTypes.list[]                |]
  , goldens "list_type"                [get| allTypes.list[].type           |]
  , goldens "list_maybeBool"           [get| allTypes.list[].maybeBool      |]
  , goldens "list_maybeInt"            [get| allTypes.list[].maybeInt       |]
  , goldens "nonexistent"              [get| allTypes.nonexistent           |]
  ]

testFromObjectAllTypes :: TestTree
testFromObjectAllTypes =
  goldens "from_object_all_types" $ map fromObj [get| allTypes.list |]
  where
    fromObj o = case [get| o.type |] of
      "bool" -> show [get| o.maybeBool! |]
      "int"  -> show [get| o.maybeInt!  |]
      "null" -> show [get| o.maybeNull  |]
      _ -> error "unreachable"

testFromObjectNested :: TestTree
testFromObjectNested = goldens "from_object_nested" $ map fromObj [get| (Nested.result).list |]
  where
    fromObj obj = case [get| obj.a |] of
      Just field -> [get| field.b |]
      Nothing    -> [get| obj.b |]

testFromObjectNamespaced :: TestTree
testFromObjectNamespaced = goldens "from_object_namespaced" $
  map fromAllTypes [get| allTypes.list |]
  ++ map fromNested [get| (Nested.result).list |]
  where
    fromAllTypes o = Text.unpack [get| o.type |]
    fromNested o = show [get| o.b |]

type NestedObject = [unwrap| (Nested.Schema).list[] |]

parseNestedObject :: NestedObject -> Int
parseNestedObject obj = fromMaybe [get| obj.b |] [get| obj.a?.b |]

nestedList :: [NestedObject]
nestedList = [get| (Nested.result).list[] |]

testUnwrapSchema :: TestTree
testUnwrapSchema = testGroup "Test unwrapping schemas"
  [ goldens "unwrap_schema_nested_list" nestedList
  , goldens "unwrap_schema_nested_object" $ map parseNestedObject nestedList
  ]
