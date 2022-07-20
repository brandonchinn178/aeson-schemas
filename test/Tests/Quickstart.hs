{-# LANGUAGE OverloadedStrings #-}

module Tests.Quickstart (test) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcess)
import Test.Tasty (TestTree)

import TestUtils (testGoldenIO)

test :: TestTree
test = testGoldenIO "README Quickstart works" "README_Quickstart.golden" $
  withSystemTempDirectory "readme-quickstart" $ \tmpdir -> do
    let testfile = tmpdir </> "readme_quickstart.hs"
    readme <- Text.readFile "README.md"
    Text.writeFile testfile (getQuickstartCode readme)
    readProcess "runghc" [testfile] ""

getQuickstartCode :: Text -> Text
getQuickstartCode = Text.unlines . getQuickstartLines . Text.lines
  where
    getQuickstartLines readmeLines =
      fromMaybe (error "Could not find Quickstart in README") $
        between (== "## Quickstart") ("## " `Text.isPrefixOf`) readmeLines
          >>= between (== "```haskell") (== "```")

    between start end xs =
      case dropWhile (not . start) xs of
        [] -> Nothing
        _ : afterStart -> Just $ takeWhile (not . end) afterStart
