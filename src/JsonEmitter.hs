{-# LANGUAGE OverloadedStrings #-}

module JsonEmitter (emitJson) where

import Types
import Analyze (extractFlatRules, runQuery1, runQuery2, runQuery3, runQuery4)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import Data.List (intersperse)

emitJson :: Maybe Int -> SandboxProfile -> Text
emitJson sel profile =
  let rules = extractFlatRules profile
      queries = case sel of
        Just 1 -> [mkQuery1 rules]
        Just 2 -> [mkQuery2 rules]
        Just 3 -> [mkQuery3 rules]
        Just 4 -> [mkQuery4 rules]
        _      -> [mkQuery1 rules, mkQuery2 rules, mkQuery3 rules, mkQuery4 rules]
  in TL.toStrict $ B.toLazyText $ jsonArray queries

mkQuery1 :: [FlatRule] -> B.Builder
mkQuery1 rules = queryObject 1
  "file-write* without container (third-party caps only)"
  (map singleRuleFinding (runQuery1 rules))

mkQuery2 :: [FlatRule] -> B.Builder
mkQuery2 rules = queryObject 2
  "file-read* without container or capabilities"
  (map singleRuleFinding (runQuery2 rules))

mkQuery3 :: [FlatRule] -> B.Builder
mkQuery3 rules = queryObject 3
  "overlapping read/write without container or capabilities"
  (map pairedRuleFinding (runQuery3 rules))

mkQuery4 :: [FlatRule] -> B.Builder
mkQuery4 rules = queryObject 4
  "mobile-path reads without container or capabilities"
  (map singleRuleFinding (runQuery4 rules))

queryObject :: Int -> Text -> [B.Builder] -> B.Builder
queryObject n desc findings =
  jsonObj [ ("query", B.fromString (show n))
          , ("description", jsonStr desc)
          , ("findings", jsonArray findings)
          ]

singleRuleFinding :: FlatRule -> B.Builder
singleRuleFinding (FlatRule _ action filters) =
  jsonObj [ ("operation", jsonStr action)
          , ("filters", jsonArray (map filterToJson filters))
          ]

pairedRuleFinding :: (FlatRule, FlatRule) -> B.Builder
pairedRuleFinding (rRule, wRule) =
  jsonObj [ ("read_rule", singleRuleFinding rRule)
          , ("write_rule", singleRuleFinding wRule)
          ]

filterToJson :: FlatFilter -> B.Builder
filterToJson ff = case ff of
  FlatSubpath path ->
    jsonObj [("type", jsonStr "subpath"), ("value", jsonStr path)]

  FlatLiteral path ->
    jsonObj [("type", jsonStr "literal"), ("value", jsonStr path)]

  FlatRegex pat ->
    jsonObj [("type", jsonStr "regex"), ("value", jsonStr (T.replace "\\." "[.]" pat))]

  FlatEntitlement name [] ->
    jsonObj [ ("type", jsonStr "require-entitlement")
            , ("name", jsonStr name)
            , ("filters", jsonArray [])
            ]

  FlatEntitlement name vals ->
    jsonObj [ ("type", jsonStr "require-entitlement")
            , ("name", jsonStr name)
            , ("filters", jsonArray (map filterToJson vals))
            ]

  FlatGeneric name [FlatStringVal val]
    | T.toLower name == "extension" ->
      jsonObj [("type", jsonStr "extension"), ("value", jsonStr val)]
    | T.toLower name == "vnode-type" ->
      jsonObj [("type", jsonStr "vnode-type"), ("value", jsonStr val)]

  FlatGeneric name [FlatAtomVal val]
    | T.toLower name == "extension" ->
      jsonObj [("type", jsonStr "extension"), ("value", jsonStr val)]
    | T.toLower name == "vnode-type" ->
      jsonObj [("type", jsonStr "vnode-type"), ("value", jsonStr val)]

  FlatGeneric name args ->
    jsonObj [ ("type", jsonStr "generic")
            , ("name", jsonStr name)
            , ("args", jsonArray (map filterValueToJson args))
            ]

  FlatDebugMode ->
    jsonObj [("type", jsonStr "debug-mode")]

  FlatRequireNot inner ->
    jsonObj [("type", jsonStr "require-not"), ("filter", filterToJson inner)]

  FlatRaw raw ->
    jsonObj [("type", jsonStr "raw"), ("value", jsonStr raw)]

filterValueToJson :: FlatFilterValue -> B.Builder
filterValueToJson fv = case fv of
  FlatStringVal s ->
    jsonObj [("type", jsonStr "string"), ("value", jsonStr s)]

  FlatRegexVal r ->
    jsonObj [("type", jsonStr "regex"), ("value", jsonStr (T.replace "\\." "[.]" r))]

  FlatBoolVal b ->
    jsonObj [("type", jsonStr "bool"), ("value", if b then "true" else "false")]

  FlatNumberVal n ->
    jsonObj [("type", jsonStr "number"), ("value", B.fromString (show n))]

  FlatAtomVal a ->
    jsonObj [("type", jsonStr "atom"), ("value", jsonStr (T.toLower a))]

  FlatFilterArg name args ->
    jsonObj [ ("type", jsonStr "compound")
            , ("value", jsonStr (T.toLower name))
            , ("args", jsonArray (map filterValueToJson args))
            ]

-- Minimal JSON builders (no aeson dependency needed)

jsonObj :: [(Text, B.Builder)] -> B.Builder
jsonObj pairs = "{" <> mconcat (intersperse "," (map renderPair pairs)) <> "}"
  where renderPair (k, v) = jsonStr k <> ":" <> v

jsonArray :: [B.Builder] -> B.Builder
jsonArray items = "[" <> mconcat (intersperse "," items) <> "]"

jsonStr :: Text -> B.Builder
jsonStr t = "\"" <> B.fromText (escapeJson t) <> "\""

escapeJson :: Text -> Text
escapeJson = T.concatMap escapeChar
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c    = T.singleton c
