module PureScript.Lint.Internal.RuleSet
  ( FlatRules
  , Rule
  , class ToRule
  , flattenRules
  , group
  , rule
  ) where

import Data.Array (foldl, snoc) as Array
import PureScript.Lint.Rule (DeclarationRule, ExprRule, ModuleRule)
import PureScript.Lint.Rule.Survey (PackageRule, WorkspaceRule)

-- | One entry in a rule set: a rule at any of the levels, or a named
-- | group of them.
data Rule
  = RuleModule ModuleRule
  | RuleDeclaration DeclarationRule
  | RuleExpr ExprRule
  | RulePackage PackageRule
  | RuleWorkspace WorkspaceRule
  | RuleGroup String (Array Rule)

-- | Lets `rule` accept a rule from any level without the caller saying
-- | which level it is.
class ToRule r where
  rule :: r -> Rule

instance ToRule ModuleRule where
  rule = RuleModule

instance ToRule DeclarationRule where
  rule = RuleDeclaration

instance ToRule ExprRule where
  rule = RuleExpr

instance ToRule PackageRule where
  rule = RulePackage

instance ToRule WorkspaceRule where
  rule = RuleWorkspace

-- | Name a section of a rule set. Presentation only - it groups the
-- | output and has no effect on which rules run.
group :: String -> Array Rule -> Rule
group = RuleGroup

-- | A rule set sorted into one array per level, which is the shape the
-- | runner needs.
type FlatRules =
  { modules :: Array ModuleRule
  , declarations :: Array DeclarationRule
  , expressions :: Array ExprRule
  , packages :: Array PackageRule
  , workspaces :: Array WorkspaceRule
  }

-- | Sort a rule set by level, discarding grouping.
flattenRules :: Array Rule -> FlatRules
flattenRules rules =
  let
    step acc = case _ of
      RuleModule r -> acc { modules = Array.snoc acc.modules r }
      RuleDeclaration r -> acc { declarations = Array.snoc acc.declarations r }
      RuleExpr r -> acc { expressions = Array.snoc acc.expressions r }
      RulePackage r -> acc { packages = Array.snoc acc.packages r }
      RuleWorkspace r -> acc { workspaces = Array.snoc acc.workspaces r }
      RuleGroup _ rs -> Array.foldl step acc rs

    empty = { modules: [], declarations: [], expressions: [], packages: [], workspaces: [] }
  in
    Array.foldl step empty rules
