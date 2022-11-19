{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Aeson (decode)
import Data.Maybe (fromJust)

import Data.Aeson.Schema

o :: Object [schema| { foo: Bool } |]
o = fromJust $ decode "{ \"foo\": true }"

result :: _
result = [get| o.missing |]

main :: IO ()
main = pure ()
