module PureScript.Lint.RuleSet.Do
  ( discard
  , group
  , rule
  ) where

import Prelude

import Data.Array (singleton) as Array
import PureScript.Lint.RuleSet (class ToRule, Rule)
import PureScript.Lint.RuleSet (group, rule) as RuleSet

discard :: ∀ m. Semigroup m => m -> (Unit -> m) -> m
discard x k = x <> k unit

rule :: ∀ r. ToRule r => r -> Array Rule
rule = Array.singleton <<< RuleSet.rule

group :: String -> Array Rule -> Array Rule
group name rules = Array.singleton (RuleSet.group name rules)
