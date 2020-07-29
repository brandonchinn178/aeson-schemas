{-|
Module      :  Data.Aeson.Schema.TH.Parse
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for parsing input text in QuasiQuoters.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Aeson.Schema.TH.Parse where

#if !MIN_VERSION_megaparsec(6,4,0)
import Control.Applicative (empty)
#endif
import Control.Monad (void)
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

#if !MIN_VERSION_megaparsec(7,0,0)
errorBundlePretty :: (Ord t, ShowToken t, ShowErrorComponent e) => ParseError t e -> String
errorBundlePretty = parseErrorPretty
#endif

parse :: MonadFail m => Parser a -> String -> m a
parse parser s = either (fail . errorBundlePretty) return $ runParser parser s s

{- Parser primitives -}

type GetterOps = [GetterOperation]

data GetterOperation
  = GetterKey String
  | GetterList [GetterOps] -- ^ Invariant: needs to be non-empty
  | GetterTuple [GetterOps] -- ^ Invariant: needs to be non-empty
  | GetterBang
  | GetterMapList
  | GetterMapMaybe
  | GetterBranch Int
  deriving (Show)

parseGetterOp :: Parser GetterOperation
parseGetterOp = choice
  [ lexeme "!" $> GetterBang
  , lexeme "[]" $> GetterMapList
  , lexeme "?" $> GetterMapMaybe
  , lexeme "@" *> (GetterBranch . read <$> some digitChar)
  , optional (lexeme ".") *> choice
      [ GetterKey <$> jsonKey
      , fmap GetterList $ between (lexeme "[") (lexeme "]") $ some parseGetterOp `sepBy1` lexeme ","
      , fmap GetterTuple $ between (lexeme "(") (lexeme ")") $ some parseGetterOp `sepBy1` lexeme ","
      ]
  ]

parseSchemaDef :: Parser SchemaDef
parseSchemaDef = parseSchemaDefWithUnions
  where
    parseSchemaDefWithUnions =
      let parseSchemaUnion [] = error "Parsed no schema definitions" -- should not happen; sepBy1 returns one or more
          parseSchemaUnion [schemaDef'] = schemaDef'
          parseSchemaUnion schemaDefs = SchemaDefUnion schemaDefs
      in fmap parseSchemaUnion $ parseSchemaDefWithoutUnions `sepBy1` lexeme "|"

    parseSchemaDefWithoutUnions = choice
      [ between (lexeme "{") (lexeme "}") $ SchemaDefObj <$> parseSchemaDefObjItems
      , between (lexeme "(") (lexeme ")") parseSchemaDefWithUnions
      , lexeme "Maybe" *> (SchemaDefMaybe <$> parseSchemaDefWithoutUnions)
      , lexeme "Try" *> (SchemaDefTry <$> parseSchemaDefWithoutUnions)
      , lexeme "List" *> (SchemaDefList <$> parseSchemaDefWithoutUnions)
      , SchemaDefType <$> identifier upperChar
      , SchemaDefInclude <$> parseSchemaReference
      ] <* space -- allow any trailing spaces

    parseSchemaDefObjItems = parseSchemaDefObjItem `sepEndBy1` lexeme ","
    parseSchemaDefObjItem = choice
      [ SchemaDefObjPair <$> parseSchemaDefPair
      , SchemaDefObjExtend <$> parseSchemaReference
      ] <* space -- allow any trailing spaces
    parseSchemaDefPair = do
      key <- choice
        [ SchemaDefObjKeyNormal <$> jsonKey
        , SchemaDefObjKeyPhantom <$> between (lexeme' "[") (lexeme' "]") jsonKey'
        ]
      lexeme ":"
      value <- parseSchemaDefWithUnions
      return (key, value)
    parseSchemaReference = char '#' *> namespacedIdentifier upperChar

-- | A Haskell identifier, with the given first character.
identifier :: Parser Char -> Parser String
identifier start = (:) <$> start <*> many (alphaNumChar <|> char '\'')

lexeme :: String -> Parser ()
lexeme = lexemeUsingLineComment $ L.skipLineComment "//"

-- | Same as 'lexeme', but without parsing comments.
lexeme' :: String -> Parser ()
lexeme' = lexemeUsingLineComment empty

lexemeUsingLineComment :: Parser () -> String -> Parser ()
lexemeUsingLineComment lineComment = void . L.lexeme (L.space space1 lineComment empty) . string

-- | Parses `identifier`, but if parentheses are provided, parses a namespaced identifier.
namespacedIdentifier :: Parser Char -> Parser String
namespacedIdentifier start = choice [lexeme "(" *> namespaced <* lexeme ")", ident]
  where
    ident = identifier start
    namespaced = intercalate "." <$> manyAndEnd (identifier upperChar <* lexeme ".") ident
    manyAndEnd p end = choice
      [ try $ p >>= \x -> (x:) <$> manyAndEnd p end
      , (:[]) <$> end
      ]

-- | An optionally quoted JSON key.
jsonKey :: Parser String
jsonKey = choice [char '"' *> jsonKey' <* char '"', jsonKey']

-- | A string that can be used as a JSON key.
jsonKey' :: Parser String
jsonKey' = some $ choice
  [ try $ char '\\' *> anySingle'
  , noneOf $ [' ', '\\', '"'] ++ schemaChars ++ getChars
  ]
  where
#if MIN_VERSION_megaparsec(7,0,0)
    anySingle' = anySingle
#else
    anySingle' = anyChar
#endif

    -- characters that cause ambiguity when parsing 'get' expressions
    getChars = "!?[](),.@"
    -- characters that should not indicate the start of a key when parsing 'schema' definitions
    schemaChars = ":{}#"

{- SchemaDef -}

data SchemaDef
  = SchemaDefType String
  | SchemaDefMaybe SchemaDef
  | SchemaDefTry SchemaDef
  | SchemaDefList SchemaDef
  | SchemaDefInclude String
  | SchemaDefObj [SchemaDefObjItem]
  | SchemaDefUnion [SchemaDef]
  deriving (Show)

data SchemaDefObjItem
  = SchemaDefObjPair (SchemaDefObjKey, SchemaDef)
  | SchemaDefObjExtend String
  deriving (Show)

data SchemaDefObjKey
  = SchemaDefObjKeyNormal String
  | SchemaDefObjKeyPhantom String
  deriving (Show)

schemaDef :: Parser SchemaDef
schemaDef = do
  space
  def <- parseSchemaDef
  space
  void eof
  return def

{- GetterExp -}

data GetterExp = GetterExp
  { start     :: Maybe String
  , getterOps :: GetterOps
  } deriving (Show)

getterExp :: Parser GetterExp
getterExp = do
  space
  start <- optional $ namespacedIdentifier lowerChar
  getterOps <- some parseGetterOp
  space
  void eof
  return GetterExp{..}

{- UnwrapSchema -}

data UnwrapSchema = UnwrapSchema
  { startSchema :: String
  , getterOps   :: GetterOps
  } deriving (Show)

unwrapSchema :: Parser UnwrapSchema
unwrapSchema = do
  space
  startSchema <- namespacedIdentifier upperChar
  getterOps <- some parseGetterOp
  space
  void eof
  return UnwrapSchema{..}
