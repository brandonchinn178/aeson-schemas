#!/usr/bin/env stack
{- stack runghc --package Cabal -}

import Data.List (intercalate)
import Distribution.Package (packageVersion)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import qualified Distribution.Verbosity as Verbosity
import Distribution.Version (versionNumbers)

main :: IO ()
main = do
  packageDesc <- readGenericPackageDescription Verbosity.silent "aeson-schemas.cabal"
  let version = intercalate "." . map show . versionNumbers . packageVersion $ packageDesc
  setOutput "version" version

{- |
Set output for a GitHub action.
https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#setting-an-output-parameter
-}
setOutput :: String -> String -> IO ()
setOutput name value = putStrLn $ "::set-output name=" ++ name ++ "::" ++ value
