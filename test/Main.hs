{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Aeson (ToJSON, decode, eitherDecode, encode)
import Data.Aeson.Schema (Object, get, unwrap)
import Data.Aeson.Schema.Utils.Sum (SumType(..), fromSumType)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Char (toLower, toUpper)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import qualified Data.Text as Text
import Language.Haskell.TH.TestUtils (tryQErr')
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
    (arbitrary, elements, infiniteListOf, oneof, testProperty, (===))
import Text.RawString.QQ (r)

import qualified AllTypes
import Enums
import qualified Nested
import Schema
import SumType
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
  , testSchemaDef
  , testMkGetter
  , testEnumTH
  , testSumType
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

testSchemaDef :: TestTree
testSchemaDef = testGroup "Test generating schema definitions"
  [ goldens' "schema_def_bool" $(showSchema [r| { a: Bool } |])
  , goldens' "schema_def_int" $(showSchema [r| { a: Int } |])
  , goldens' "schema_def_double" $(showSchema [r| { foo123: Double } |])
  , goldens' "schema_def_text" $(showSchema [r| { some_text: Text } |])
  , goldens' "schema_def_custom" $(showSchema [r| { status: Status } |])
  , goldens' "schema_def_maybe" $(showSchema [r| { a: Maybe Int } |])
  , goldens' "schema_def_list" $(showSchema [r| { a: List Int } |])
  , goldens' "schema_def_obj" $(showSchema [r| { a: { b: Int } } |])
  , goldens' "schema_def_maybe_obj" $(showSchema [r| { a: Maybe { b: Int } } |])
  , goldens' "schema_def_list_obj" $(showSchema [r| { a: List { b: Int } } |])
  , goldens' "schema_def_import_user" $(showSchema [r| { user: #UserSchema } |])
  , goldens' "schema_def_extend" $(showSchema [r| { a: Int, #(Schema.MySchema) } |])
  , goldens' "schema_def_shadow" $(showSchema [r| { extra: Bool, #(Schema.MySchema) } |])
  , goldens' "schema_def_union" $(showSchema [r| { a: List Int | Text } |])
  , goldens' "schema_def_union_grouped" $(showSchema [r| { a: List (Int | Text) } |])
  -- bad schema definitions
  , goldens' "schema_def_duplicate" $(tryQErr' $ showSchema [r| { a: Int, a: Bool } |])
  , goldens' "schema_def_duplicate_phantom" $(tryQErr' $ showSchema [r| { a: Int, [a]: { b: Bool } } |])
  , goldens' "schema_def_duplicate_extend" $(tryQErr' $ showSchema [r| { #MySchema, #MySchema2 } |])
  , goldens' "schema_def_not_object" $(tryQErr' $ showSchema [r| List { a: Int } |])
  , goldens' "schema_def_unknown_type" $(tryQErr' $ showSchema [r| HelloWorld |])
  , goldens' "schema_def_invalid_extend" $(tryQErr' $ showSchema [r| { #Int } |])
  , goldens' "schema_def_nonobject_phantom" $(tryQErr' $ showSchema [r| { [a]: Int } |])
  ]

testMkGetter :: TestTree
testMkGetter = testGroup "Test the mkGetter helper"
  [ goldens "getter_all_types_list" list
  , goldens "getter_all_types_list_item" $ map getType list
  ]
  where
    list :: [AllTypes.ListItem]
    list = AllTypes.getList AllTypes.result

    getType :: AllTypes.ListItem -> Text.Text
    getType = [get| .type |]

testEnumTH :: TestTree
testEnumTH = testGroup "Test the Enum TH helpers"
  -- State
  [ testProperty "mkEnum decode is case insensitive" $ do
      (val, enumVal) <- elements
        [ ("OPEN", OPEN)
        , ("CLOSED", CLOSED)
        ]
      casedVal <- randomlyCased val
      return $ decode (ByteString.pack $ show casedVal) === Just enumVal
  , testCase "mkEnum encode keeps case of constructor" $ do
      encode OPEN @?= "\"OPEN\""
      encode CLOSED @?= "\"CLOSED\""
  , testProperty "mkEnum: (fromJust . decode . encode) === id" $ do
      enumVal <- elements [OPEN, CLOSED]
      return $ (decode . encode) enumVal === Just enumVal

  -- Color
  , testProperty "genFromJSONEnum decode is case insensitive" $ do
      (val, enumVal) <- elements
        [ ("Red", Red)
        , ("LightBlue", LightBlue)
        , ("Yellow", Yellow)
        , ("DarkGreen", DarkGreen)
        , ("Black", Black)
        , ("JustABitOffWhite", JustABitOffWhite)
        ]
      casedVal <- randomlyCased val
      return $ decode (ByteString.pack $ show casedVal) === Just enumVal
  , testCase "genToJSONEnum encode keeps case of constructor" $ do
      encode Red @?= "\"Red\""
      encode LightBlue @?= "\"LightBlue\""
      encode Yellow @?= "\"Yellow\""
      encode DarkGreen @?= "\"DarkGreen\""
      encode Black @?= "\"Black\""
      encode JustABitOffWhite @?= "\"JustABitOffWhite\""
  , testProperty "genFromJSONEnum + genToJSONEnum: (fromJust . decode . encode) === id" $ do
      enumVal <- elements
        [ Red
        , LightBlue
        , Yellow
        , DarkGreen
        , Black
        , JustABitOffWhite
        ]
      return $ (decode . encode) enumVal === Just enumVal
  ]
  where
    randomlyCased s = do
      caseFuncs <- infiniteListOf $ elements [toLower, toUpper]
      return $ zipWith ($) caseFuncs s

testSumType :: TestTree
testSumType = testGroup "Test the SumType helper"
  [ testCase "Sanity checks" $
      let values =
            [ Here True
            , Here False
            , There (Here 1)
            , There (Here 10)
            , There (There (Here []))
            , There (There (Here ["a"]))
            ] :: [SpecialJSON]
      in values @?= values
  , testGroup "Decode SumType"
    [ testProperty "branch 1" $ \(b :: Bool) ->
        toSpecialJSON b === Right (Here b)
    , testProperty "branch 2" $ \(x :: Int) ->
        toSpecialJSON x === Right (There (Here x))
    , testProperty "branch 3" $ \(l :: [String]) ->
        toSpecialJSON l === Right (There (There (Here l)))
    , testCase "invalid SumType" $
        toSpecialJSON [True] @?= Left "Error in $: Could not parse sum type"
    ]
  , testGroup "fromSumType"
    [ testProperty "branch 0 valid" $ \b ->
        fromSumType (Proxy @0) (Here b :: SpecialJSON) === Just b
    , testProperty "branch 0 invalid" $ do
        value <- oneof
          [ There . Here <$> arbitrary
          , There . There . Here <$> arbitrary
          ]
        return $ fromSumType (Proxy @0) (value :: SpecialJSON) === Nothing
    , testProperty "branch 1 valid" $ \x ->
        fromSumType (Proxy @1) (There (Here x) :: SpecialJSON) === Just x
    , testProperty "branch 1 invalid" $ do
        value <- oneof
          [ Here <$> arbitrary
          , There . There . Here <$> arbitrary
          ]
        return $ fromSumType (Proxy @1) (value :: SpecialJSON) === Nothing
    , testProperty "branch 2 valid" $ \l ->
        fromSumType (Proxy @2) (There (There (Here l)) :: SpecialJSON) === Just l
    , testProperty "branch 2 invalid" $ do
        value <- oneof
          [ Here <$> arbitrary
          , There . Here <$> arbitrary
          ]
        return $ fromSumType (Proxy @2) (value :: SpecialJSON) === Nothing
    ]
  ]
  where
    toSpecialJSON :: ToJSON a => a -> Either String SpecialJSON
    toSpecialJSON = eitherDecode . encode
