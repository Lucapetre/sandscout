module Main where

import Parser (parseSandboxProfile)
import PrologEmitter (emitProlog)
import Analyze
import Types (SandboxProfile(..))

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty)

data Mode
  = ModeAST
  | ModePrologFacts
  | ModeQuery
  deriving (Eq, Show)

parseArgs :: [String] -> Either String (Mode, FilePath)
parseArgs ["--ast", fp]          = Right (ModeAST, fp)
parseArgs ["--prolog-facts", fp] = Right (ModePrologFacts, fp)
parseArgs ["--query", fp]        = Right (ModeQuery, fp)
parseArgs [fp]                   = Right (ModeQuery, fp)
parseArgs _                      = Left usage
  where

    usage = unlines
      [ "Usage: sandscout [MODE] <path-to-sb-profile>"
      , ""
      , "Modes:"
      , "  --ast            Print the parsed sandbox profile AST"
      , "  --prolog-facts   Output all flattened allow/deny Prolog facts"
      , "  --query          Run analysis queries and print results (default)"
      ]

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left msg -> do
      putStrLn msg
      exitFailure
    Right (mode, filePath) -> do
      content <- TIO.readFile filePath
      case parseSandboxProfile filePath content of
        Left err -> do
          putStrLn (errorBundlePretty err)
          exitFailure
        Right profile ->
          runMode mode profile

runMode :: Mode -> SandboxProfile -> IO ()
runMode ModeAST profile = do
  mapM_ print (profileRules profile)

runMode ModePrologFacts profile = do
  TIO.putStr (emitProlog profile)

runMode ModeQuery profile = do
  let rules = extractFlatRules profile

  putStrLn "=== Query 1: file-write* without container (third-party caps only) ==="
  mapM_ (TIO.putStrLn . formatQuery1) (runQuery1 rules)
  putStrLn ""

  putStrLn "=== Query 2: file-read* without container or capabilities ==="
  mapM_ (TIO.putStrLn . formatQuery2) (runQuery2 rules)
  putStrLn ""

  putStrLn "=== Query 3: overlapping read/write without container or capabilities ==="
  mapM_ (TIO.putStrLn . formatQuery3) (runQuery3 rules)
  putStrLn ""

  putStrLn "=== Query 4: mobile-path reads without container or capabilities ==="
  mapM_ (TIO.putStrLn . formatQuery4) (runQuery4 rules)