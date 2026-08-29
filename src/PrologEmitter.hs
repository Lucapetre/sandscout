{-# LANGUAGE OverloadedStrings #-}

module PrologEmitter
  ( emitProlog
  , flattenRule
  , flattenFilter
  , flattenFilterValue
  , renderFlatRule
  , renderFlatFilter
  , renderFlatFilterValue
  , negateFlatFilter
  , renderDecision
  , quote
  ) where

import Types
import Render

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
      _ | all isRegexVal args && length args > 1 ->
          map (\r -> [FlatGeneric name [flattenFilterValue r]]) args
      [AtomVal sub, StringVal val] ->
          [[FlatGeneric name [FlatFilterArg sub [FlatStringVal val]]]]
      _ ->
          [[FlatGeneric name (map flattenFilterValue args)]]
    where
      isRegexVal (RegexVal _) = True
      isRegexVal _            = False

  BooleanConst b ->
    if b then [[FlatRaw "#t"]] else [[FlatRaw "#f"]]

flattenFilterValue :: FilterValue -> FlatFilterValue
flattenFilterValue fv = case fv of
  StringVal s  -> FlatStringVal s
  RegexVal r   -> FlatRegexVal r
  BoolVal b    -> FlatBoolVal b
  NumberVal n  -> FlatNumberVal n
  AtomVal a    -> FlatAtomVal a
  GenericVal name args -> FlatFilterArg name (map flattenFilterValue args)

negateFlatFilter :: FlatFilter -> FlatFilter
negateFlatFilter (FlatRequireNot inner) = inner
negateFlatFilter ff = FlatRequireNot ff