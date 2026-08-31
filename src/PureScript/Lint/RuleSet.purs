-- | A rule set is an `Array Rule`. Build one with `rule` and `group`,
-- | or with the `do` syntax in `PureScript.Lint.RuleSet.Do`.
module PureScript.Lint.RuleSet (module Exports) where

import PureScript.Lint.Internal.RuleSet (class ToRule, Rule, group, rule) as Exports
