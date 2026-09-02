module Main where

import Parser (parseSandboxProfile)
import PrologEmitter (emitProlog)
import Analyze
import JsonEmitter (emitJson)
import Types (SandboxProfile(..), FlatRule)

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty)

data Mode
  = ModeAST
  | ModePrologFacts
  | ModeQuery (Maybe Int)
  | ModeJson (Maybe Int)
  deriving (Eq, Show)

parseArgs :: [String] -> Either String (Mode, FilePath)
parseArgs ["--ast", fp]            = Right (ModeAST, fp)
parseArgs ["--prolog-facts", fp]   = Right (ModePrologFacts, fp)
parseArgs ["--query", fp]          = Right (ModeQuery Nothing, fp)
parseArgs ["--query", n, fp]       = parseQueryNum ModeQuery n fp
parseArgs ["--json", fp]           = Right (ModeJson Nothing, fp)
parseArgs ["--json", n, fp]        = parseQueryNum ModeJson n fp
parseArgs [fp]                     = Right (ModeQuery Nothing, fp)
parseArgs _                        = Left usage
  where
    usage = unlines
      [ "Usage: sandscout [MODE] <path-to-sb-profile>"
      , ""
      , "Modes:"
      , "  --ast              Print the parsed sandbox profile AST"
      , "  --prolog-facts     Output all flattened allow/deny Prolog facts"
      , "  --query [1-4]      Run analysis queries and print results (default)"
      , "  --json  [1-4]      Output query results as JSON"
      , "                     Omit number to run all queries"
      ]

parseQueryNum :: (Maybe Int -> Mode) -> String -> FilePath -> Either String (Mode, FilePath)
parseQueryNum mode n fp = case reads n of
  [(num, "")] | num >= 1 && num <= 4 -> Right (mode (Just num), fp)
  _ -> Left $ "Invalid query number: " ++ n ++ " (must be 1-4)"

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
runMode ModeAST profile =
  mapM_ print (profileRules profile)

runMode ModePrologFacts profile =
  TIO.putStr (emitProlog profile)

runMode (ModeQuery sel) profile = do
  let rules = extractFlatRules profile
      runQ :: String -> (a -> T.Text) -> ([FlatRule] -> [a]) -> IO ()
      runQ header formatter query = do
        putStrLn $ "=== " ++ header ++ " ==="
        mapM_ (TIO.putStrLn . formatter) (query rules)
        putStrLn ""

      q1 = runQ "Query 1: file-write* without container (third-party caps only)" formatQuery1 runQuery1
      q2 = runQ "Query 2: file-read* without container or capabilities"          formatQuery2 runQuery2
      q3 = runQ "Query 3: overlapping read/write without container or capabilities" formatQuery3 runQuery3
      q4 = runQ "Query 4: mobile-path reads without container or capabilities"    formatQuery4 runQuery4

  case sel of
    Just 1 -> q1
    Just 2 -> q2
    Just 3 -> q3
    Just 4 -> q4
    _      -> q1 >> q2 >> q3 >> q4

runMode (ModeJson sel) profile =
  TIO.putStrLn (emitJson sel profile)