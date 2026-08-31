module PureScript.Lint.RuleSet
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

data Rule
  = RuleModule ModuleRule
  | RuleDeclaration DeclarationRule
  | RuleExpr ExprRule
  | RulePackage PackageRule
  | RuleWorkspace WorkspaceRule
  | RuleGroup String (Array Rule)

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

group :: String -> Array Rule -> Rule
group = RuleGroup

type FlatRules =
  { modules :: Array ModuleRule
  , declarations :: Array DeclarationRule
  , expressions :: Array ExprRule
  , packages :: Array PackageRule
  , workspaces :: Array WorkspaceRule
  }

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
