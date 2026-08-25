{-# LANGUAGE OverloadedStrings #-}

module PrologEmitter
  ( emitProlog
  , flattenRule
  , flattenFilter
  , flattenFilterValue
  , renderFlatRule
  , renderFlatFilter
  , renderFlatFilterValue
  ) where

import Types

import Data.Text (Text)
import qualified Data.Text as T

emitProlog :: SandboxProfile -> Text
emitProlog profile =
  T.unlines $ concatMap (map renderFlatRule . flattenRule) (profileRules profile)

flattenRule :: Rule -> [FlatRule]
flattenRule (Rule dec act []) =
  [FlatRule dec (T.replace "*" "STAR" act) []]
flattenRule (Rule dec act filters) =
  let action = T.replace "*" "STAR" act
      flattenedGroups = concatMap flattenFilter filters
  in  map (FlatRule dec action) flattenedGroups

flattenFilter :: Filter -> [[FlatFilter]]
flattenFilter f = case f of
  Subpath path ->
    [[FlatSubpath path]]

  Literal path ->
    [[FlatLiteral path]]

  Regex pat ->
    [[FlatRegex pat]]

  DebugMode ->
    [[FlatDebugMode]]

  RequireNot inner ->
    sequence $ map (map negateFlatFilter) (flattenFilter inner)

  RequireAny filters ->
    concatMap flattenFilter filters

  RequireAll filters ->
    let flatFiltersComb = sequence $ map flattenFilter filters
    in map concat flatFiltersComb

  RequireEntitlement name entFilters ->
    let flatFilter = flattenFilter (RequireAll entFilters)
    in if flatFilter == [[]] then
         [[FlatEntitlement name []]]
       else
         map (\conj -> [FlatEntitlement name conj]) flatFilter

  GenericFilter name args ->
    case args of
      -- Expand multiple regex arguments into separate branches
      _ | all isRegexVal args && length args > 1 ->
          map (\r -> [FlatGeneric name [flattenFilterValue r]]) args
      -- Nested otherType (e.g. local ip "*:*")
      [AtomVal sub, StringVal val] ->
          [[FlatGeneric name [FlatFilterArg sub [FlatStringVal val]]]]
      _ ->
          [[FlatGeneric name (map flattenFilterValue args)]]
    where
      isRegexVal (RegexVal _) = True
      isRegexVal _            = False

  BooleanConst b ->
    if b then [[FlatRaw "#t"]] else [[FlatRaw "#f"]]

renderFlatRule :: FlatRule -> Text
renderFlatRule (FlatRule dec action filters) =
  renderDecision dec <> "(" <> action <> ", [" <>
  T.intercalate "," (map renderFlatFilter filters) <>
  "])."

renderFlatFilter :: FlatFilter -> Text
renderFlatFilter ff = case ff of
  FlatSubpath path ->
    "subpath(" <> quote path <> ")"

  FlatLiteral path ->
    "literal(" <> quote path <> ")"

  FlatRegex pat ->
    "regex(" <> quote (T.replace "\\." "[.]" pat) <> "/i)"

  FlatEntitlement name [] ->
    "require-entitlement(" <> quote name <> ",[])"

  FlatEntitlement name vals ->
    "require-entitlement(" <> quote name <> ",[" <>
    T.intercalate "," (map renderFlatFilter vals) <>
    "])"

  -- Tratare specială pentru cazul fsctl-command(_IO "h" 32) -> fsctl-command("_io","h",32)
  FlatGeneric "fsctl-command" [FlatFilterArg io [FlatStringVal char, FlatNumberVal num]] ->
    "fsctl-command(" <> quote (T.toLower io) <> "," <> quote char <> "," <> T.pack (show num) <> ")"

  FlatGeneric name args ->
    name <> "(" <> T.intercalate "," (map renderFlatFilterValue args) <> ")"

  FlatDebugMode ->
    "debug-mode"

  FlatRequireNot (FlatEntitlement name []) ->
    "require-not(require-entitlement(" <> quote name <> "))"

  FlatRequireNot inner ->
    "require-not(" <> renderFlatFilter inner <> ")"

  FlatRaw raw ->
    raw

flattenFilterValue :: FilterValue -> FlatFilterValue
flattenFilterValue fv = case fv of
  StringVal s  -> FlatStringVal s
  RegexVal r   -> FlatRegexVal r
  BoolVal b    -> FlatBoolVal b
  NumberVal n  -> FlatNumberVal n
  AtomVal a    -> FlatAtomVal a
  GenericVal name args -> FlatFilterArg name (map flattenFilterValue args)

renderFlatFilterValue :: FlatFilterValue -> Text
renderFlatFilterValue fv = case fv of
  FlatStringVal s  -> quote s
  FlatRegexVal r   -> quote (T.replace "\\." "[.]" r) <> "/i"
  FlatBoolVal b    -> if b then "#t" else "#f"
  FlatNumberVal n  -> T.pack (show n)
  FlatAtomVal a    -> T.toLower a
  FlatFilterArg name args ->
    T.toLower name <> "(" <> T.intercalate "," (map renderFlatFilterValue args) <> ")"

negateFlatFilter :: FlatFilter -> FlatFilter
negateFlatFilter (FlatRequireNot inner) = inner
negateFlatFilter ff = FlatRequireNot ff

renderDecision :: Decision -> Text
renderDecision Allow = "allow"
renderDecision Deny  = "deny"

quote :: Text -> Text
quote t = "\"" <> t <> "\""