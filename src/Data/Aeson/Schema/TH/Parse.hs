{-|
Module      :  Data.Aeson.Schema.TH.Parse
Maintainer  :  Brandon Chinn <brandon@leapyear.io>
Stability   :  experimental
Portability :  portable

Definitions for parsing input text in QuasiQuoters.
-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Aeson.Schema.TH.Parse where

import Control.Monad (void)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Void (Void)
import Language.Haskell.TH.Syntax (Lift)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parse :: Monad m => Parser a -> String -> m a
parse parser s = either (fail . errorBundlePretty) return $ runParser parser s s

{- GetterOps -}

type GetterOps = [GetterOperation]

data GetterOperation
  = GetterKey String
  | GetterList [GetterOps]
  | GetterTuple [GetterOps]
  | GetterBang
  | GetterMapList
  | GetterMapMaybe
  deriving (Show,Lift)

showGetterOps :: GetterOps -> String
showGetterOps = concatMap showGetterOp
  where
    showGetterOp = \case
      GetterKey key -> '.':key
      GetterList elems -> ".[" ++ intercalate "," (map showGetterOps elems) ++ "]"
      GetterTuple elems -> ".(" ++ intercalate "," (map showGetterOps elems) ++ ")"
      GetterBang -> "!"
      GetterMapList -> "[]"
      GetterMapMaybe -> "?"

{- Parser primitives -}

parseGetterOp :: Parser GetterOperation
parseGetterOp = choice
  [ lexeme "!" $> GetterBang
  , lexeme "[]" $> GetterMapList
  , lexeme "?" $> GetterMapMaybe
  , optional (lexeme ".") *> choice
      [ GetterKey <$> identifier lowerChar
      , fmap GetterList $ between (lexeme "[") (lexeme "]") $ some parseGetterOp `sepBy1` lexeme ","
      , fmap GetterTuple $ between (lexeme "(") (lexeme ")") $ some parseGetterOp `sepBy1` lexeme ","
      ]
  ]

parseSchemaDef :: Parser SchemaDef
parseSchemaDef = choice
  [ between (lexeme "{") (lexeme "}") $ SchemaDefObj <$> parseSchemaDefObjItems
  , choice (map lexeme' mods) >>= parseSchemaDefMod
  , SchemaDefType <$> identifier upperChar
  , SchemaDefInclude <$> parseSchemaReference
  ]
  where
    mods = ["Maybe", "List"]
    parseSchemaDefMod s = SchemaDefMod s <$> parseSchemaDef
    parseSchemaDefObjItems = parseSchemaDefObjItem `sepEndBy1` lexeme ","
    parseSchemaDefObjItem = choice
      [ SchemaDefObjPair <$> parseSchemaDefPair
      , SchemaDefObjExtend <$> parseSchemaReference
      ] <* space -- allow any trailing spaces
    parseSchemaDefPair = do
      key <- quotedString
      lexeme ":"
      value <- parseSchemaDef
      return (key, value)
    parseSchemaReference = char '#' *> namespacedIdentifier upperChar

-- | A Haskell identifier, with the given first character.
identifier :: Parser Char -> Parser String
identifier start = (:) <$> start <*> many (alphaNumChar <|> char '\'')

lexeme :: String -> Parser ()
lexeme = void . lexeme'

lexeme' :: String -> Parser String
lexeme' = L.lexeme (L.space space1 empty empty) . string

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

quotedString :: Parser String
quotedString = between (char '"') (char '"') $ many $ noneOf "\""

{- SchemaDef -}

data SchemaDef
  = SchemaDefType String
  | SchemaDefMod String SchemaDef
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
  getterOps <- many parseGetterOp
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
  getterOps <- many parseGetterOp
  space
  void eof
  return UnwrapSchema{..}
