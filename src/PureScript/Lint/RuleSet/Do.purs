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

-- ## Context
--
-- Lets a rule set be written as `do` statements instead of an array
-- literal, which is what a configuration tree wants to look like - a
-- lint config reads much more like a spec tree than like data, and
-- leading commas were never the right punctuation for it.
--
-- There is no monad here, and deliberately so. PureScript desugars
-- `M.do { a; b; c }` to `M.discard a (\_ -> M.discard b (\_ -> c))`, so
-- a single `discard` that appends is the whole implementation. A
-- hand-rolled `Writer` was built and works too, at a newtype and five
-- instances, and buys only the difference between `do` and `Rules.do` -
-- worse than this on two counts. A rule set is a list, not a
-- computation: `bind` would sequence values and there are no values
-- here to sequence, so the monad hands callers power the domain does
-- not have. With only `discard` defined, `x <- ...` does not typecheck,
-- which is exactly the right constraint. And for a library meant to be
-- published, one line of syntax to learn beats one monad to learn.
--
-- `discard` has nothing to do with rules, or with arrays. Appending is
-- the only sensible thing to do with two values and no binding, so it is
-- typed at `Semigroup` - the same two lines build a `String`, a `Map` or
-- a `Set` from a `do` block. That generality has a cost worth naming:
-- this module is called `RuleSet.Do` but its `do` will happily typecheck
-- a block of `String`s, because the type no longer says "rules". The
-- function deserves the general type and the module deserves the narrow
-- one, and reconciling that means a module for two lines - which is the
-- shape `max-modules-per-namespace` exists to argue against. It stays
-- here until a second caller exists, and that is the trigger to move it.
--
-- `rule` shadows `PureScript.Lint.RuleSet.rule` at one extra `Array.singleton`,
-- because every statement in the block has to have the block's own type.
-- Import this module's `rule` and `group` unqualified and the module
-- itself qualified, and a config reads as a spec tree: `group "..."
-- Rules.do` nests exactly the way `describe`/`it` does, which is what
-- `PureScript.Lint.RuleSet.group` was reaching for when its own doc admitted
-- the name was decorative and unread. It has real callers now.
--
-- This is an ergonomic choice and nothing more. It was measured against
-- the problem that prompted it - a rule config's own `max-delimiter-run`
-- violations were all in nested *arguments*, not in the outer array - and
-- the qualified-do form would have prevented none of them. It is nicer to
-- read; it is not a fix for anything.
