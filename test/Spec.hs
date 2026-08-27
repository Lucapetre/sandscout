{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser (parseSandboxProfile)
import PrologEmitter (emitProlog)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory)
import System.FilePath ((</>), dropExtension, takeExtension)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertFailure)
import Text.Megaparsec (errorBundlePretty)
import Data.List (sort, nub)

-- | Find all .sb files in the test-cases directory that have a corresponding .pl file
discoverTestCases :: FilePath -> IO [TestTree]
discoverTestCases dir = do
  files <- listDirectory dir
  let sbFiles = [f | f <- files, takeExtension f == ".sb"]
      -- Only include .sb files that have a corresponding .pl file
      plFiles = [f | f <- files, takeExtension f == ".pl"]
      plBases = nub $ map dropExtension plFiles
      pairs   = sort [ dropExtension sb
                     | sb <- sbFiles
                     , dropExtension sb `elem` plBases
                     ]
  mapM (mkGoldenTest dir) pairs

-- | Create a single test: parse the .sb file, emit Prolog, and compare
-- the result against the .pl file (line order independent).
mkGoldenTest :: FilePath -> String -> IO TestTree
mkGoldenTest dir baseName = do
  let sbPath = dir </> baseName ++ ".sb"
      plPath = dir </> baseName ++ ".pl"
  return $ testCase baseName $ do
    sbContent <- TIO.readFile sbPath
    plExpected <- TIO.readFile plPath
    case parseSandboxProfile sbPath sbContent of
      Left err ->
        assertFailure $ "Failed to parse " ++ sbPath ++ ":\n" ++ errorBundlePretty err
      Right profile -> do
        let actual = emitProlog profile
        assertEqual
          ("Prolog output mismatch for " ++ baseName)
          (normalise plExpected)
          (normalise actual)

-- | Normalise text for comparison: trim, then sort so that line ordering differences do not cause failures.
normalise :: T.Text -> [T.Text]
normalise = sort . filter (not . T.null) . map T.stripEnd . T.lines

main :: IO ()
main = do
  tests <- discoverTestCases "test-cases"
  defaultMain $ testGroup "Tests (parse .sb -> emit .pl)" tests
