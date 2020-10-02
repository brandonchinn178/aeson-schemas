{-# LANGUAGE TemplateHaskell #-}

module Benchmarks.Get where

import Criterion.Main
import Language.Haskell.Interpreter
    (ImportList(..), ModuleImport(..), ModuleQualification(..), OptionVal(..))
import qualified Language.Haskell.Interpreter as Hint
import System.Environment (setEnv)
import System.Process (readProcess)

benchmarks :: Benchmark
benchmarks = bgroup "get expressions"
  [ bench "evaluate module with get quasiquoters" $ nfIO $ evalModuleWith "Data.Aeson.Schema" getQuasiquoters
  , bench "evaluate module with get functions" $ nfIO $ evalModuleWith "Data.Aeson.Schema.Get" getFunctions
  ]
  where
    evalModuleWith moduleWithGet exprs = fmap (either (error . show) id) $ runInterpreter $ do
      -- https://github.com/haskell-hint/hint/issues/105
      Hint.loadModules ["bench/examples/Bootstrap.hs"]
      Hint.setTopLevelModules ["Bootstrap"]

      Hint.setImportsF
        [ ModuleImport moduleWithGet (QualifiedAs $ Just "A") $ ImportList ["get"]
        , ModuleImport "Data.Aeson.Schema.Get" NotQualified $ HidingList ["get"]
        , ModuleImport "Data.Maybe" NotQualified $ ImportList ["catMaybes"]
        ]

      Hint.set
        [ Hint.languageExtensions :=
            [ Hint.DataKinds
            , Hint.QuasiQuotes
            , Hint.OverloadedLabels
            ]
        ]

      mapM Hint.eval $ mkGetExprs 100 exprs

    -- return the given number of get expressions to compile + run, drawing from the given
    -- expressions
    mkGetExprs numOfExprs exprs = take numOfExprs $ cycle exprs

    getQuasiquoters =
      [ "[A.get| result.users |]"
      , "map [A.get| .name |] $ catMaybes $ [A.get| result.users |]"
      , "[A.get| result.users[]?.name |]"
      ]

    getFunctions =
      [ "A.get (key #users) result"
      , "map (A.get (key #name)) $ catMaybes $ A.get (key #users) result"
      , "A.get (key #name . intoMaybe . intoList . key #users) result"
      ]

-- https://github.com/haskell-hint/hint/issues/104
runInterpreter :: Hint.InterpreterT IO a -> IO (Either Hint.InterpreterError a)
runInterpreter m = do
  packagePath <- head . lines <$> readProcess "stack" ["path", "--ghc-package-path"] ""
  setEnv "GHC_PACKAGE_PATH" packagePath
  Hint.runInterpreter m
