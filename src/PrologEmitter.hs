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

-- | Emit a full Prolog program from a parsed sandbox profile.
--   Each rule is flattened into one or more FlatRules.
emitProlog :: SandboxProfile -> Text
emitProlog profile =
  T.unlines $ concatMap (map renderFlatRule . flattenRule) (profileRules profile)

-- | Flatten a Rule into a list of FlatRules by expanding
--   RequireAll (cartesian product) and RequireAny (separate facts).
--
--   Example:
--     Rule Allow "file-read*" [RequireAll [Subpath "/a/", RequireAny [ext "x", ext "y"]]]
--   becomes:
--     [ FlatRule Allow "file-readSTAR" [FlatSubpath "/a/", FlatGeneric "ext" ["x"]]
--     , FlatRule Allow "file-readSTAR" [FlatSubpath "/a/", FlatGeneric "ext" ["y"]]
--     ]
flattenRule :: Rule -> [FlatRule]
flattenRule (Rule dec act filters) =
  let action = T.replace "*" "STAR" act
      -- Each top-level filter in a rule is implicitly in a RequireAny
      -- (separate facts for each top-level filter)
      flattenedGroups = concatMap flattenFilter filters
  in  map (FlatRule dec action) flattenedGroups

-- | Flatten a Filter into a list of filter-lists (disjunctive normal form).
--   Each inner list represents one conjunction (one Prolog fact's filter list).
--
--   RequireAny  [A, B]    =>  [[A], [B]]       (union)
--   RequireAll  [A, B]    =>  [[A1, B1], [A2, B1], [A1, B2], [A2, B2], ...] (cartesian product)
--   Leaf filters          =>  [[ FlatLeaf ]] (singleton)
--
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
    -- Not: cartesian product of all negated filters (DeMorgan laws)
    sequence $ map (map negateFlatFilter) (flattenFilter inner)

  RequireAny filters ->
    -- Union: each alternative produces separate facts
    concatMap flattenFilter filters

  RequireAll filters ->
    -- Intersection: cartesian product - combine every possibility from each filter
    let flatFiltersComb = sequence $ map flattenFilter filters
    -- sequence for [[a]] does cartesian product (monad magic)
    in map concat flatFiltersComb

  RequireEntitlement name entFilters ->
    -- require-entitlement("key", [entitlement-value("val")])
    let flatFilter = flattenFilter (RequireAll entFilters)
    in if flatFilter == [[]] then
         [[FlatEntitlement name []]]
       else
         [map (FlatEntitlement name) flatFilter]

  GenericFilter name args ->
    [[FlatGeneric name (map flattenFilterValue args)]]

  BooleanConst b ->
    -- TODO: this might need changing
    if b then [[FlatRaw "#t"]] else [[FlatRaw "#f"]]

-- | Render a FlatRule as a Prolog fact.
--   Format: decision(action, [filter1,filter2,...]).
renderFlatRule :: FlatRule -> Text
renderFlatRule (FlatRule dec action filters) =
  renderDecision dec <> "(" <> action <> ", [" <>
  T.intercalate "," (map renderFlatFilter filters) <>
  "])."

-- | Render a FlatFilter as a Prolog term.
renderFlatFilter :: FlatFilter -> Text
renderFlatFilter ff = case ff of
  FlatSubpath path ->
    "subpath(" <> quote path <> ")"

  FlatLiteral path ->
    "literal(" <> quote path <> ")"

  FlatRegex pat ->
    -- Prolog format: regex("^pattern$"/i)
    "regex(" <> quote (T.replace "\\." "[.]" pat) <> "/i)"

  FlatEntitlement name vals ->
    "require-entitlement(" <> name <> ",[" <>
    T.intercalate "," (map renderFlatFilter vals) <>
    "])"

  FlatGeneric name args ->
    name <> "(" <> T.intercalate "," (map renderFlatFilterValue args) <> ")"

  FlatDebugMode ->
    "debug-mode"

  FlatRequireNot inner ->
    "require-not(" <> renderFlatFilter inner <> ")"

  FlatRaw raw ->
    raw

-- | Convert a FilterValue to a FlatFilterValue.
flattenFilterValue :: FilterValue -> FlatFilterValue
flattenFilterValue fv = case fv of
  StringVal s  -> FlatStringVal s
  RegexVal r   -> FlatRegexVal r
  BoolVal b    -> FlatBoolVal b
  NumberVal n  -> FlatNumberVal n
  GenericVal name args -> FlatFilterArg name (map flattenFilterValue args)

-- | Render a FlatFilterValue as a Prolog term.
renderFlatFilterValue :: FlatFilterValue -> Text
renderFlatFilterValue fv = case fv of
  FlatStringVal s  -> quote $ T.toLower s
  FlatRegexVal r   -> quote r <> "/i"
  FlatBoolVal b    -> if b then "#t" else "#f"
  FlatNumberVal n  -> T.pack (show n)
  FlatFilterArg name args ->
    name <> "(" <> T.intercalate "," (map renderFlatFilterValue args) <> ")"

-- Helpers

negateFlatFilter :: FlatFilter -> FlatFilter
negateFlatFilter (FlatRequireNot inner) = inner
negateFlatFilter ff = FlatRequireNot ff

renderDecision :: Decision -> Text
renderDecision Allow = "allow"
renderDecision Deny  = "deny"

quote :: Text -> Text
quote t = "\"" <> t <> "\""
