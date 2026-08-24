{-# LANGUAGE OverloadedStrings #-}

module Parser( parseSandboxProfile
  , profileP
  , ruleP
  , filterP
  ) where

import Types

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

decisionP :: Parser Decision
decisionP = (symbol "allow" >> return Allow)
        <|> (symbol "deny" >> return Deny)

filterValueP :: Parser FilterValue
filterValueP = (RegexVal <$> regexPatternP)
           <|> (StringVal <$> quotedStringP)
           <|> (BoolVal <$> try boolP)
           <|> (NumberVal <$> try numberP)
           <|> (FilterVal <$> try filterP)
           <|> parens (ListVal <$> many filterValueP)
           <|> (StringVal <$> identifierP)

subpathP :: Parser Filter
subpathP = do
  _ <- symbol "subpath"
  paths <- some quotedStringP
  let norm p = if T.isSuffixOf "/" p then p else p <> "/"
  case map (Subpath . norm) paths of
    [s] -> return s
    multiple -> return $ RequireAny multiple

literalP :: Parser Filter
literalP = do
  _ <- symbol "literal"
  paths <- some quotedStringP
  case map Literal paths of
    [s] -> return s
    multiple -> return $ RequireAny multiple

regexP :: Parser Filter
regexP = do
  _ <- symbol "regex"
  pats <- some regexPatternP
  case map Regex pats of
    [s] -> return s
    multiple -> return $ RequireAny multiple

requireEntitlementP :: Parser Filter
requireEntitlementP = do
  _ <- symbol "require-entitlement"
  name <- quotedStringP
  vals <- many filterP
  return $ RequireEntitlement name vals

filterP :: Parser Filter
filterP = parens $
      subpathP
  <|> literalP
  <|> regexP
  <|> (symbol "require-all" >> RequireAll <$> many filterP)
  <|> (symbol "require-any" >> RequireAny <$> many filterP)
  <|> (symbol "require-not" >> RequireNot <$> filterP)
  <|> requireEntitlementP
  <|> (symbol "debug-mode" >> return DebugMode)
  <|> do
        name <- identifierP
        args <- many filterValueP
        return $ GenericFilter name args

ruleP :: Parser Rule
ruleP = parens $ do
  dec <- decisionP
  act <- identifierP
  filts <- many filterP
  let sanitizedAct = T.replace "*" "STAR" act
  return $ Rule dec sanitizedAct filts

versionP :: Parser Text
versionP = parens $ do
  _ <- symbol "version"
  (T.pack . show <$> numberP) <|> identifierP

defaultDecisionP :: Parser Decision
defaultDecisionP = parens $ do
  dec <- decisionP
  _ <- symbol "default"
  return dec

profileP :: Parser SandboxProfile
profileP = do
  sc
  ver <- optional versionP
  defDec <- optional defaultDecisionP
  rls <- many ruleP
  eof
  let finalVer = maybe "1" id ver
  let finalDef = maybe Deny id defDec
  return $ SandboxProfile finalVer finalDef rls

parseSandboxProfile :: String -> Text -> Either (ParseErrorBundle Text Void) SandboxProfile
parseSandboxProfile = runParser profileP
