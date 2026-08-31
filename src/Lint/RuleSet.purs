-- | A rule set is an `Array Rule`. Build one with `rule` and `group`,
-- | or with the `do` syntax in `Lint.RuleSet.Do`.
module Lint.RuleSet (module Exports) where

import Lint.Internal.RuleSet (class ToRule, Rule, group, rule) as Exports
