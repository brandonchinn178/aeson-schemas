{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Aeson.Schema (Object, get, unwrap)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Language.Haskell.TH.TestUtils (tryQErr')
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

import qualified AllTypes
import qualified Nested
import qualified Tests.EnumTH
import qualified Tests.MkGetterQQ
import qualified Tests.SchemaQQ
import qualified Tests.SumType
import Util

allTypes :: Object AllTypes.Schema
allTypes = AllTypes.result

main :: IO ()
main = defaultMain $ testGroup "aeson-schemas"
  [ testGetterExp
  , testFromObjectAllTypes
  , testFromObjectNested
  , testFromObjectNamespaced
  , testUnwrapSchema
  , Tests.SchemaQQ.test
  , Tests.MkGetterQQ.test
  , Tests.EnumTH.test
  , Tests.SumType.test
  ]

goldens' :: String -> String -> TestTree
goldens' name = goldenVsString name fp . pure . ByteString.pack
  where
    fp = "test/goldens/" ++ name ++ ".golden"

goldens :: Show s => String -> s -> TestTree
goldens name = goldens' name . show

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
  , goldens "tryObj"                   [get| allTypes.tryObject             |]
  , goldens "tryObjNull"               [get| allTypes.tryObjectNull         |]
  , goldens "tryObj_bang"              [get| allTypes.tryObject!            |]
  , goldens "tryObj_a"                 [get| allTypes.tryObject?.a          |]
  , goldens "tryObj_bang_a"            [get| allTypes.tryObject!.a          |]
  , goldens "tryObjNull_a"             [get| allTypes.tryObjectNull?.a      |]
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
  , goldens "union"                    [get| allTypes.union                 |]
  , goldens "union_0"                  [get| allTypes.union[]@0             |]
  , goldens "union_0_a"                [get| allTypes.union[]@0?.a          |]
  , goldens "union_1"                  [get| allTypes.union[]@1             |]
  , goldens "union_2"                  [get| allTypes.union[]@2             |]
  , goldens "phantom"                  [get| allTypes.phantom.keyForPhantom |]
  -- bad 'get' expressions
  , goldens' "maybeListNull_bang" $(getError [get| (AllTypes.result).maybeListNull! |])
#if MIN_VERSION_megaparsec(7,0,0)
  , goldens' "get_empty" $(tryQErr' $ showGet "")
  , goldens' "get_just_start" $(tryQErr' $ showGet "allTypes")
#endif
  , goldens' "get_ops_after_tuple" $(tryQErr' $ showGet "allTypes.(bool,int).foo")
  , goldens' "get_ops_after_list" $(tryQErr' $ showGet "allTypes.[int,int2].foo")
  ]

testFromObjectAllTypes :: TestTree
testFromObjectAllTypes =
  goldens "from_object_all_types" $ map fromObj [get| allTypes.list |]
  where
    fromObj o = case Text.unpack [get| o.type |] of
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
  [ goldens' "unwrap_schema" $(showUnwrap "(Nested.Schema).list[]")
  , goldens "unwrap_schema_nested_list" nestedList
  , goldens "unwrap_schema_nested_object" $ map parseNestedObject nestedList
  -- bad unwrap types
  , goldens' "unwrap_schema_bad_bang" $(tryQErr' $ showUnwrap "(AllTypes.Schema).list[]!")
  , goldens' "unwrap_schema_bad_question" $(tryQErr' $ showUnwrap "(AllTypes.Schema).list[]?")
  , goldens' "unwrap_schema_bad_list" $(tryQErr' $ showUnwrap "(AllTypes.Schema).list[][]")
  , goldens' "unwrap_schema_bad_key" $(tryQErr' $ showUnwrap "(AllTypes.Schema).list.a")
  , goldens' "unwrap_schema_bad_branch" $(tryQErr' $ showUnwrap "(AllTypes.Schema).list@0")
  , goldens' "unwrap_schema_branch_out_of_bounds" $(tryQErr' $ showUnwrap "(AllTypes.Schema).union[]@10")
  ]
