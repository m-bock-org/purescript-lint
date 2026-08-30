module PureScript.Lint.RuleSet
  ( FlatRules
  , LintPhase
  , LintRuleSet
  , Rule
  , class ToRule
  , flattenRules
  , group
  , rule
  ) where

import Data.Array (foldl, snoc) as Array
import PureScript.Lint.Rule (DeclarationRule, ExprRule, GlobalExemption, ModuleRule)
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

type LintPhase =
  { name :: String
  , rules :: Array Rule
  }

type LintRuleSet =
  { globalExclude :: Array GlobalExemption
  , phases :: Array LintPhase
  }

-- A `LintPhase` is a named set of rules you can run on its own. The
-- split exists because a rule's answer is only worth acting on once it
-- is stable: a formatting complaint raised while a function is still
-- being restructured gets invalidated by the next edit, while the same
-- complaint on settled code is a real finding. Same rule, better signal,
-- purely from when it fires.
--
-- Phases are plain data - a name and an array - rather than a closed
-- type, and deliberately so. This library should not decide that every
-- project has exactly three stages, or that they are called design,
-- convention and polish; that is one workflow, not a fact about linting.
--
-- Nor are they a chain. An earlier attempt made `Phase` an `Ord` sum and
-- ran everything up to a cutoff, which quietly assumes each set nests
-- inside the next - true for design/convention/polish, false in general
-- (`pre-commit` and `ci` overlap without either containing the other).
-- A config that *does* want nesting writes `design <> convention`, which
-- is ordinary array concatenation and needs nothing from here.
--
-- The same rule may appear in several phases, and that is not a mistake
-- to warn about: a cheap high-value check can reasonably belong to every
-- set. The cost of allowing it is that a rule listed in no phase never
-- runs, silently - which is why the runner prints how many rules a phase
-- actually contains.
--
--
-- Split out of `PureScript.Lint.Rule` (2026-08-29). Assembling a rule set is
-- a different job from defining what a rule is, and it is the only part
-- that has to know about every rule kind at once - which is why adding
-- the survey kinds is what pushed `Rule` over its limits. Keeping the
-- assembly here also breaks what would otherwise be a cycle: the survey
-- module needs `Rule`'s vocabulary, and `Rule` would have needed the
-- survey types to build `Rule`'s own constructors.
