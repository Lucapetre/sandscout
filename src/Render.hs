{-# LANGUAGE OverloadedStrings #-}

module Render
  ( RenderMode(..)
  , renderFlatRule
  , renderFlatFilter
  , renderFlatFilterUnquoted
  , renderFlatFilterValue
  , renderFlatFilterValueUnquoted
  , renderDecision
  , quote
  ) where

import Types
import Data.Text (Text)
import qualified Data.Text as T

data RenderMode = Quoted | Unquoted

renderFlatRule :: FlatRule -> Text
renderFlatRule (FlatRule dec action filters) =
  renderDecision dec <> "(" <> action <> ", [" <>
  T.intercalate "," (map renderFlatFilter filters) <>
  "])."

renderFlatFilter :: FlatFilter -> Text
renderFlatFilter = renderFlatFilterMode Quoted

renderFlatFilterUnquoted :: FlatFilter -> Text
renderFlatFilterUnquoted = renderFlatFilterMode Unquoted

renderFlatFilterValue :: FlatFilterValue -> Text
renderFlatFilterValue = renderFlatFilterValueMode Quoted

renderFlatFilterValueUnquoted :: FlatFilterValue -> Text
renderFlatFilterValueUnquoted = renderFlatFilterValueMode Unquoted

renderFlatFilterMode :: RenderMode -> FlatFilter -> Text
renderFlatFilterMode mode ff = case ff of
  FlatSubpath path ->
    "subpath(" <> applyQuote path <> ")"

  FlatLiteral path ->
    "literal(" <> applyQuote path <> ")"

  FlatRegex pat ->
    let fixed = T.replace "\\." "[.]" pat
    in case mode of
      Quoted -> "regex(" <> quote fixed <> "/i)"
      Unquoted ->
        if T.isSuffixOf "$" fixed || T.isSuffixOf "/" fixed
        then "regex(" <> fixed <> " / i)"
        else "regex(" <> fixed <> "/i)"

  FlatEntitlement name [] ->
    case mode of
      Quoted -> "require-entitlement(" <> quote name <> ",[])"
      Unquoted -> "require-entitlement(" <> name <> ")"

  FlatEntitlement name vals ->
    "require-entitlement(" <> applyQuote name <> ",[" <>
    T.intercalate "," (map (renderFlatFilterMode mode) vals) <>
    "])"

  FlatGeneric "fsctl-command" [FlatFilterArg io [FlatStringVal char, FlatNumberVal num]] ->
    case mode of
      Quoted -> "fsctl-command(" <> quote (T.toLower io) <> "," <> quote char <> "," <> T.pack (show num) <> ")"
      Unquoted -> "fsctl-command(" <> T.toLower io <> "," <> char <> "," <> T.pack (show num) <> ")"

  FlatGeneric name args ->
    name <> "(" <> T.intercalate "," (map (renderFlatFilterValueMode mode) args) <> ")"

  FlatDebugMode ->
    "debug-mode"

  FlatRequireNot (FlatEntitlement name []) ->
    case mode of
      Quoted -> "require-not(require-entitlement(" <> quote name <> "))"
      Unquoted -> "require-not(require-entitlement(" <> name <> "))"

  FlatRequireNot inner ->
    "require-not(" <> renderFlatFilterMode mode inner <> ")"

  FlatRaw raw ->
    raw
  where
    applyQuote val = case mode of
      Quoted -> quote val
      Unquoted -> val

renderFlatFilterValueMode :: RenderMode -> FlatFilterValue -> Text
renderFlatFilterValueMode mode fv = case fv of
  FlatStringVal s  -> case mode of
    Quoted -> quote s
    Unquoted -> s
  FlatRegexVal r   ->
    let fixed = T.replace "\\." "[.]" r
    in case mode of
      Quoted -> quote fixed <> "/i"
      Unquoted ->
        if T.isSuffixOf "$" fixed || T.isSuffixOf "/" fixed
        then fixed <> " / i"
        else fixed <> "/i"
  FlatBoolVal b    -> if b then "#t" else "#f"
  FlatNumberVal n  -> T.pack (show n)
  FlatAtomVal a    -> T.toLower a
  FlatFilterArg name args ->
    T.toLower name <> "(" <> T.intercalate "," (map (renderFlatFilterValueMode mode) args) <> ")"

renderDecision :: Decision -> Text
renderDecision Allow = "allow"
renderDecision Deny  = "deny"

quote :: Text -> Text
quote t = "\"" <> t <> "\""