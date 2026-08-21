module Types where

import Data.Text (Text)

data Decision
  = Allow
  | Deny
  deriving (Show, Eq)

data FilterValue
  = StringVal Text
  | RegexVal Text
  | BoolVal Bool
  | NumberVal Integer
  | FilterVal Filter
  | ListVal [FilterValue]
  deriving (Show, Eq)

data Filter
  = Subpath Text
  | Literal Text
  | Regex Text
  | RequireEntitlement Text [FilterValue]
  | RequireAll [Filter]
  | RequireAny [Filter]
  | RequireNot Filter
  | GenericFilter Text [FilterValue]
  | DebugMode
  | BooleanConst Bool
  deriving (Show, Eq)

data Rule = Rule
  { ruleDecision :: Decision
  , ruleAction   :: Text
  , ruleFilters  :: [Filter]
  } deriving (Show, Eq)

data SandboxProfile = SandboxProfile
  { profileVersion  :: Text
  , defaultDecision :: Decision
  , profileRules    :: [Rule]
  } deriving (Show, Eq)

data FlatFilter
  = FlatSubpath Text
  | FlatLiteral Text
  | FlatRegex Text
  | FlatEntitlement Text [FilterValue]
  | FlatGeneric Text [FilterValue]
  | FlatDebugMode
  | FlatRequireNot FlatFilter
  | FlatRaw Text
  deriving (Show, Eq)

data FlatRule = FlatRule
  { flatDecision :: Decision
  , flatAction   :: Text
  , flatFilters  :: [FlatFilter]
  } deriving (Show, Eq)