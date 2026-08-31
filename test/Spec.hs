{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Parser (parseSandboxProfile)
import PrologEmitter (emitProlog)
import Analyze
import Types (FlatRule)

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

-- | Build a query test: parse the .sb input, run the query, format
-- each result, and compare against the expected .out file.
mkQueryTest :: String -> FilePath -> FilePath -> (a -> T.Text) -> ([FlatRule] -> [a]) -> IO TestTree
mkQueryTest label sbPath outPath formatter query = do
  return $ testCase label $ do
    sbContent <- TIO.readFile sbPath
    outExpected <- TIO.readFile outPath
    case parseSandboxProfile sbPath sbContent of
      Left err ->
        assertFailure $ "Failed to parse " ++ sbPath ++ ":\n" ++ errorBundlePretty err
      Right profile -> do
        let rules  = extractFlatRules profile
            actual = T.unlines $ map formatter (query rules)
        assertEqual
          ("Query output mismatch for " ++ label)
          (normalise outExpected)
          (normalise actual)

-- | Normalise text for comparison: trim, then sort so that line ordering differences do not cause failures.
normalise :: T.Text -> [T.Text]
normalise = sort . filter (not . T.null) . map T.stripEnd . T.lines

main :: IO ()
main = do
  emitTests <- discoverTestCases "test-cases"

  let sbFile = "test-cases" </> "containerBetterGraphProcess.sb"
      outDir = "outputFromQueries"

  q1 <- mkQueryTest "query1" sbFile (outDir </> "query1.out") formatQuery1 runQuery1
  q2 <- mkQueryTest "query2" sbFile (outDir </> "query2.out") formatQuery2 runQuery2
  q4 <- mkQueryTest "query4" sbFile (outDir </> "query4.out") formatQuery4 runQuery4

  defaultMain $ testGroup "Sandscout"
    [ testGroup "Prolog emission (parse .sb -> emit .pl)" emitTests
    , testGroup "Query output (containerBetterGraphProcess)" [q1, q2, q4]
    ]

