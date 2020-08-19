{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

import Criterion.Main
import Language.Haskell.TH (Type)
import Language.Haskell.TH.Quote (QuasiQuoter(quoteType))
import Language.Haskell.TH.TestUtils
    (QMode(..), QState(..), loadNames, runTestQ)

import Data.Aeson.Schema (schema)
import DeepSeq ()
import Schemas

type Schema1 = $(genSchema 1)
type Schema5 = $(genSchema 5)
type Schema10 = $(genSchema 10)
type Schema100 = $(genSchema 100)

$(return [])

main :: IO ()
main = defaultMain
  [ schemaQQBenchmarks
  ]

schemaQQBenchmarks :: Benchmark
schemaQQBenchmarks = bgroup "schema quasiquoter"
  [ schemaKeysBenchmarks
  , extendSchemaBenchmarks
  ]
  where
    schemaKeysBenchmarks = bgroup "# of keys" $
      map
      (\n -> bench (show n) $ nf runSchema $ genSchemaDef n)
      [1, 5, 10, 100]

    extendSchemaBenchmarks = bgroup "Extend schema with given # of keys"
      [ bench "0" $ nf runSchema "{ a: Int }"
      , bench "1" $ nf runSchema "{ a: Int, #Schema1 }"
      , bench "5" $ nf runSchema "{ a: Int, #Schema5 }"
      , bench "10" $ nf runSchema "{ a: Int, #Schema10 }"
      , bench "100" $ nf runSchema "{ a: Int, #Schema100 }"
      ]

runSchema :: String -> Type
runSchema = runTestQ qstate . quoteType schema
  where
    qstate = QState
      { mode = MockQ
      , knownNames =
          [ ("Schema1", ''Schema1)
          , ("Schema5", ''Schema5)
          , ("Schema10", ''Schema10)
          , ("Schema100", ''Schema100)
          ]
      , reifyInfo = $(loadNames [''Schema1, ''Schema5, ''Schema10, ''Schema100])
      }
