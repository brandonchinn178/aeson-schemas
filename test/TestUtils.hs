{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module TestUtils
  ( ShowSchemaResult(..)
  , json
  ) where

import Data.Aeson (eitherDecode)
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import Data.Typeable (Typeable, typeRep)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (lift)

import Data.Aeson.Schema (Object)
import qualified Data.Aeson.Schema.Internal as Internal

{- ShowSchemaResult -}

class ShowSchemaResult a where
  showSchemaResult :: String

instance Typeable schema => ShowSchemaResult (Object schema) where
  showSchemaResult = "Object (" ++ Internal.showSchema @schema ++ ")"

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
