{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

import Criterion.Main
import Language.Haskell.TH (Type)
import Language.Haskell.TH.Quote (QuasiQuoter(quoteType))
import Language.Haskell.TH.TestUtils
    (QMode(..), QState(..), loadNames, runTestQ)

import qualified Data.Aeson.Schema
import DeepSeq ()
import Schemas

main :: IO ()
main = defaultMain
  [ schemaQQBenchmarks
  ]

schemaQQBenchmarks :: Benchmark
schemaQQBenchmarks = bgroup "schema quasiquoter"
  [ byKeys
  , byKeysInExtended
  ]
  where
    byKeys = bgroup "# of keys" $
      flip map [1, 5, 10, 100] $ \n ->
        let schemaDef = genSchemaDef $ keysTo n
        in bench (show n) $ nf runSchema schemaDef

    byKeysInExtended = bgroup "Extend schema with given # of keys" $
      flip map sizedSchemas $ \(n, schema) ->
        let schemaDef = genSchemaDef [Field "a" "Int", Ref schema]
        in bench (show n) $ nf runSchema schemaDef

runSchema :: String -> Type
runSchema = runTestQ qstate . quoteType Data.Aeson.Schema.schema
  where
    qstate = QState
      { mode = MockQ
      , knownNames = sizedSchemasNames
      , reifyInfo = $(loadNames $ map snd sizedSchemasNames)
      }
