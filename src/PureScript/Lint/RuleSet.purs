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

-- A rule set is an `Array Rule` and nothing more. A project that wants
-- more than one - a quick set while restructuring, the full set in CI -
-- writes more than one, and a set that builds on another writes
-- `quick <> rest`, which is ordinary array concatenation. That only
-- works because there is no wrapper: a record would have to be merged
-- field by field instead.
--
-- This used to be a `LintRuleSet` record carrying named `phases`, and
-- the runner took a phase name to
-- select one (2026-08-31). Two things were wrong with it. The engine
-- never used the structure - it looked one entry up by name and worked
-- on that array, so it was a `Map String (Array Rule)` spelled as a
-- record. And the lookup could not fail: an unknown name selected an
-- empty rule set, so the linter ran nothing, reported zero violations
-- and exited green. A misspelt `--phase` passed CI. Selecting a rule
-- set by a string from argv is a real need, but it belongs to whoever
-- owns the command line, where an unknown name can be an error instead
-- of silence.
--
-- `globalExclude` went to `runLinter` at the same time. It is how one
-- run is configured, not what the rules are, and it sat beside
-- `excludeRules` - already a runner argument - so the two halves of
-- "skip this" were split across two places for no reason.
--
-- Split out of `PureScript.Lint.Rule` (2026-08-29). Assembling a rule set is
-- a different job from defining what a rule is, and it is the only part
-- that has to know about every rule kind at once - which is why adding
-- the survey kinds is what pushed `Rule` over its limits. Keeping the
-- assembly here also breaks what would otherwise be a cycle: the survey
-- module needs `Rule`'s vocabulary, and `Rule` would have needed the
-- survey types to build `Rule`'s own constructors.
