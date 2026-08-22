{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Types

import Control.Applicative (empty, (<|>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

quotedStringP :: Parser Text
quotedStringP = lexeme $ do
  _ <- char '"'
  content <- takeWhileP Nothing (/= '"')
  _ <- char '"'
  return content

regexPatternP :: Parser Text
regexPatternP = lexeme $ do
  _ <- string "#\""
  content <- takeWhileP Nothing (/= '"')
  _ <- char '"'
  return content

identifierChar :: Char -> Bool
identifierChar c = not (c `elem` [' ', '\t', '\r', '\n', '(', ')', '"', ';'])

identifierP :: Parser Text
identifierP = lexeme $ takeWhile1P Nothing identifierChar

numberP :: Parser Integer
numberP = lexeme L.decimal

boolP :: Parser Bool
boolP = lexeme $
      (string "#t" >> return True)
  <|> (string "#f" >> return False)
  <|> (string "true" >> return True)
  <|> (string "false" >> return False)

decisionP :: Parser Decision
decisionP = (symbol "allow" >> return Allow)
        <|> (symbol "deny" >> return Deny)