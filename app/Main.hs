module Main (main) where

import Parser
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- TIO.readFile filePath
      case parseSandboxProfile filePath content of
        Left err -> do
          putStrLn (errorBundlePretty err)
          exitFailure
        Right profile -> do
          print profile
    _ -> do
      putStrLn "Usage: sandscout <file.sb>"
      exitFailure