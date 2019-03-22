{-|
Module      :  Data.Aeson.Schema.TH.Parse
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for parsing input text in QuasiQuoters.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Aeson.Schema.TH.Parse where

import Control.Monad (void)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Aeson.Schema.TH.Utils (GetterOperation(..), GetterOps)

type Parser = Parsec Void String

parse :: Monad m => Parser a -> String -> m a
parse parser s = either (fail . errorBundlePretty) return $ runParser parser s s

{- Parser primitives -}

parseGetterOp :: Parser GetterOperation
parseGetterOp = choice
  [ lexeme "!" $> GetterBang
  , lexeme "[]" $> GetterMapList
  , lexeme "?" $> GetterMapMaybe
  , optional (lexeme ".") *> choice
      [ GetterKey <$> jsonKey
      , fmap GetterList $ between (lexeme "[") (lexeme "]") $ some parseGetterOp `sepBy1` lexeme ","
      , fmap GetterTuple $ between (lexeme "(") (lexeme ")") $ some parseGetterOp `sepBy1` lexeme ","
      ]
  ]

parseSchemaDef :: Parser SchemaDef
parseSchemaDef = choice
  [ between (lexeme "{") (lexeme "}") $ SchemaDefObj <$> parseSchemaDefObjItems
  , lexeme "Maybe" *> (SchemaDefMaybe <$> parseSchemaDef)
  , lexeme "List" *> (SchemaDefList <$> parseSchemaDef)
  , SchemaDefType <$> identifier upperChar
  , SchemaDefInclude <$> parseSchemaReference
  ]
  where
    parseSchemaDefObjItems = parseSchemaDefObjItem `sepEndBy1` lexeme ","
    parseSchemaDefObjItem = choice
      [ SchemaDefObjPair <$> parseSchemaDefPair
      , SchemaDefObjExtend <$> parseSchemaReference
      ] <* space -- allow any trailing spaces
    parseSchemaDefPair = do
      key <- jsonKey
      lexeme ":"
      value <- parseSchemaDef
      return (key, value)
    parseSchemaReference = char '#' *> namespacedIdentifier upperChar

-- | A Haskell identifier, with the given first character.
identifier :: Parser Char -> Parser String
identifier start = (:) <$> start <*> many (alphaNumChar <|> char '\'')

lexeme :: String -> Parser ()
lexeme = void . L.lexeme (L.space space1 (L.skipLineComment "//") empty) . string

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

-- | A string that can be used as a JSON key.
jsonKey :: Parser String
jsonKey = some $ noneOf $ " " ++ schemaChars ++ getChars
  where
    -- characters that cause ambiguity when parsing 'get' expressions
    getChars = "!?[](),."
    -- characters that should not indicate the start of a key when parsing 'schema' definitions
    schemaChars = ":{}#"

{- SchemaDef -}

data SchemaDef
  = SchemaDefType String
  | SchemaDefMaybe SchemaDef
  | SchemaDefList SchemaDef
  | SchemaDefInclude String
  | SchemaDefObj [SchemaDefObjItem]
  deriving (Show)

data SchemaDefObjItem
  = SchemaDefObjPair (String, SchemaDef)
  | SchemaDefObjExtend String
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
