-- | What a rule is: a record carrying a name, a description, an example
-- | of each side, and a function from one piece of syntax to a verdict.
-- |
-- | Pick the level by the constructor you wrap it in - `perExpr`,
-- | `perDecl` or `perModule` - which decides what the function is handed
-- | and how often it runs. Reach for the smallest one that can see the
-- | answer.
module PureScript.Lint.Rule (module Exports) where

import PureScript.Lint.Internal.Rule
  ( class HasExclude
  , class RuleOptions
  , DeclarationLint
  , DeclarationRule
  , ExprLint
  , ExprRule
  , LintContext
  , LintExemption
  , LintResult
  , ModuleExemption
  , ModuleKind(..)
  , ModuleLint
  , ModuleRule
  , dedent
  , disabled
  , exclude
  , fixed
  , perDecl
  , perExpr
  , perModule
  , violations
  , withHint
  ) as Exports
