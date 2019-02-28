{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Util where

import Data.Aeson.Schema (decodeWithSchema)
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.ByteString.Lazy.Char8 as Char8L
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)

getMockedResult :: FilePath -> ExpQ
getMockedResult fp = do
  contents <- runIO $ ByteStringL.readFile fp
  [| either error id $ decodeWithSchema $ Char8L.pack $(lift $ Char8L.unpack contents) |]
