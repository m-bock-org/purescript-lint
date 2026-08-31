-- | A rule set written as `do` statements rather than an array literal.
-- |
-- | Import this module twice - the names unqualified, the module
-- | qualified - so a rule set reads as a nested tree:
-- |
-- | ```purescript
-- | import PureScript.Lint.RuleSet.Do (group, rule)
-- | import PureScript.Lint.RuleSet.Do as Rules
-- |
-- | myRules :: Array Rule
-- | myRules = Rules.do
-- |   group "Declarations" Rules.do
-- |     rule $ perDecl (maxFunctionArity 4)
-- | ```
-- |
-- | Only `discard` is defined, so `x <- ...` does not typecheck. A rule
-- | set is a list, not a computation, and there is nothing to bind.
module PureScript.Lint.RuleSet.Do
  ( discard
  , group
  , rule
  ) where

import Prelude

import Data.Array (singleton) as Array
import PureScript.Lint.Internal.RuleSet (class ToRule, Rule)
import PureScript.Lint.Internal.RuleSet (group, rule) as RuleSet

-- | What `Rules.do` desugars to: append each statement to the next.
discard :: ∀ m. Semigroup m => m -> (Unit -> m) -> m
discard x k = x <> k unit

-- | `PureScript.Lint.RuleSet.rule`, as a one-element block.
rule :: ∀ r. ToRule r => r -> Array Rule
rule = Array.singleton <<< RuleSet.rule

-- | `PureScript.Lint.RuleSet.group`, as a one-element block.
group :: String -> Array Rule -> Array Rule
group name rules = Array.singleton (RuleSet.group name rules)
