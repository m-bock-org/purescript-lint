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
module PureScript.Lint.RuleSet.Do
  ( discard
  , group
  , rule
  ) where

import Prelude

import Data.Array (singleton) as Array
import PureScript.Lint.Internal.RuleSet (class ToRule, Rule)
import PureScript.Lint.Internal.RuleSet (group, rule) as RuleSet

-- | Required for `Rules.do`.
discard :: ∀ m. Semigroup m => m -> (Unit -> m) -> m
discard x k = x <> k unit

-- | Put one rule into the set.
rule :: ∀ r. ToRule r => r -> Array Rule
rule = Array.singleton <<< RuleSet.rule

-- | Name a section of the set.
group :: String -> Array Rule -> Array Rule
group name rules = Array.singleton (RuleSet.group name rules)
