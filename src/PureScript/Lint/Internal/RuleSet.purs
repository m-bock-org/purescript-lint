module PureScript.Lint.Internal.RuleSet
  ( FlatRules
  , Rule
  , class ToRule
  , flattenRules
  , group
  , rule
  ) where

import Data.Array (foldl, snoc) as Array
import PureScript.Lint.Internal.Survey (PackageRule, WorkspaceRule)
import PureScript.Lint.Internal.Rule (DeclarationRule, ExprRule, Grouped, ModuleRule)

-- | One entry in a rule set. Build one with `rule` or `group`.
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
  -- | Put a rule of any level into a rule set.
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
  { modules :: Array (Grouped ModuleRule)
  , declarations :: Array (Grouped DeclarationRule)
  , expressions :: Array (Grouped ExprRule)
  , packages :: Array (Grouped PackageRule)
  , workspaces :: Array (Grouped WorkspaceRule)
  }

-- | Sort a rule set by level, keeping the groups each rule sits under.
flattenRules :: Array Rule -> FlatRules
flattenRules rules =
  let
    step groups acc = case _ of
      RuleModule r -> acc { modules = Array.snoc acc.modules { groups, rule: r } }
      RuleDeclaration r ->
        acc { declarations = Array.snoc acc.declarations { groups, rule: r } }
      RuleExpr r -> acc { expressions = Array.snoc acc.expressions { groups, rule: r } }
      RulePackage r -> acc { packages = Array.snoc acc.packages { groups, rule: r } }
      RuleWorkspace r ->
        acc { workspaces = Array.snoc acc.workspaces { groups, rule: r } }
      RuleGroup name rs -> Array.foldl (step (Array.snoc groups name)) acc rs

    empty = { modules: [], declarations: [], expressions: [], packages: [], workspaces: [] }
  in
    Array.foldl (step []) empty rules
