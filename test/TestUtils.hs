{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module TestUtils
  ( ShowSchemaResult(..)
  , json
  , parseValue
  , parseObject
  ) where

import Data.Aeson (FromJSON(..), Value, eitherDecode)
import Data.Aeson.Types (parseEither)
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import Data.Typeable (Typeable, typeRep)
import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (lift)

import Data.Aeson.Schema (Object, schema)
import qualified Data.Aeson.Schema.Internal as Internal

{- ShowSchemaResult -}

class ShowSchemaResult a where
  showSchemaResult :: String

instance Typeable (Internal.ToSchemaObject schema) => ShowSchemaResult (Object schema) where
  showSchemaResult = "Object (" ++ Internal.showSchemaType @(Internal.ToSchemaObject schema) ++ ")"

instance ShowSchemaResult a => ShowSchemaResult [a] where
  showSchemaResult = "[" ++ showSchemaResult @a ++ "]"

instance {-# OVERLAPPABLE #-} Typeable a => ShowSchemaResult a where
  showSchemaResult = show $ typeRep (Proxy @a)

{- Loading JSON data -}

json :: QuasiQuoter
json = QuasiQuoter
  { quoteExp = \s -> [| (either error id . eitherDecode . fromString) $(lift s) |]
  , quotePat = error "Cannot use the 'json' QuasiQuoter for patterns"
  , quoteType = error "Cannot use the 'json' QuasiQuoter for types"
  , quoteDec = error "Cannot use the 'json' QuasiQuoter for declarations"
  }

parseValue :: FromJSON a => Value -> a
parseValue = either error id . parseEither parseJSON

parseObject :: String -> ExpQ
parseObject schemaString = [| parseValue :: Value -> Object $schemaType |]
  where
    schemaType = quoteType schema schemaString
