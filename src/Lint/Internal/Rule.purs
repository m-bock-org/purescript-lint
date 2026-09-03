module Lint.Internal.Rule
  ( Guarded
  , DeclarationLint
  , Examples
  , DeclarationRule
  , ExprLint
  , ExprRule
  , ModuleExemption
  , ModuleKind(..)
  , LintContext
  , LintExemption
  , LintResult
  , ModuleLint
  , ModuleRule
  , Finding
  , Grouped
  , RuleInfo
  , RuleOutcome
  , ruleInfo
  , class HasExclude
  , class RuleLike
  , class RuleOptions
  , disabled
  , exclude
  , fixed
  , perDecl
  , perDecl_
  , perExpr
  , perExpr_
  , perModule
  , perModule_
  , ruleCheck
  , ruleDisabled
  , ruleExclude
  , runRules
  , skipWhen
  , violations
  , withHint
  ) where

import Prelude

import Data.Array (filter, find, foldl, head, init, last, tail) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (fromArray, toArray) as NEA
import Data.Foldable (minimum) as Foldable
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe, maybe) as Maybe
import Data.String (Pattern(..)) as Str
import Data.String.CodeUnits (drop, dropWhile, length) as Str
import Data.String.Common (joinWith, split, trim) as Str
import Node.Path (FilePath)
import PureScript.CST.Types (Declaration, Expr, Module) as CST

-- | A rule's verdict. Opaque: build one with `violations` or `fixed`.
data LintResult a
  = Passed
  | Violations (NonEmptyArray String) (Maybe String)
  | Fixed a

-- | Every finding a rule made. A rule passes by finding nothing, so
-- | `violations []` is how it says so.
violations :: ∀ a. Array String -> LintResult a
violations found =
  Maybe.maybe Passed (\vs -> Violations vs Nothing) (NEA.fromArray found)

-- | A rule that rewrote what it was given, rather than complaining
-- | about it.
fixed :: ∀ a. a -> LintResult a
fixed = Fixed

-- | Attach a suggestion to every finding of a result. The hint belongs
-- | to the rule, so it reads the same however many things were found.
withHint :: ∀ a. String -> LintResult a -> LintResult a
withHint hint = case _ of
  Violations found _ -> Violations found (Just hint)
  other -> other

-- | Which of a package's two module trees a module came from.
data ModuleKind = SourceModule | TestModule

derive instance eqModuleKind :: Eq ModuleKind

instance showModuleKind :: Show ModuleKind where
  show = case _ of
    SourceModule -> "SourceModule"
    TestModule -> "TestModule"

-- | Where a checked value came from. A `Module` knows nothing about
-- | its own path or which package it belongs to, so a rule that needs
-- | either reads it here.
type LintContext =
  { packageName :: String
  , moduleName :: String
  , declarationName :: Maybe String
  , path :: FilePath
  , kind :: ModuleKind
  }

-- | What a rule looks like on both sides, and the setting those examples
-- | are read against: two nested lambdas are fine at a depth of two and a
-- | violation at one, so the setting travels with them.
-- |
-- | `printConfig` is a function of the setting rather than a stored
-- | string, so it cannot name a different one. `Just <<< show` where the
-- | value speaks for itself, words where the number needs a noun, and
-- | `\_ -> Nothing` where there is nothing to configure.
type Examples cfg =
  { config :: cfg
  , printConfig :: cfg -> Maybe String
  , good :: Array String
  , bad :: Array String
  }

-- | A rule that sees one whole module.
type ModuleLint cfg =
  { name :: String
  , description :: String
  , examples :: Maybe (Examples cfg)
  , rule :: cfg -> LintContext -> CST.Module Void -> LintResult (CST.Module Void)
  }

-- | A rule that sees one top-level declaration.
type DeclarationLint cfg =
  { name :: String
  , description :: String
  , examples :: Maybe (Examples cfg)
  , rule :: cfg -> LintContext -> CST.Declaration Void -> LintResult (CST.Declaration Void)
  }

-- | A rule that sees one expression, anywhere inside a declaration.
type ExprLint cfg =
  { name :: String
  , description :: String
  , examples :: Maybe (Examples cfg)
  , rule :: cfg -> LintContext -> CST.Expr Void -> LintResult (CST.Expr Void)
  }

-- | A named reason one rule should skip a particular value. The name
-- | reads as documentation where the exemption is applied.
type LintExemption a =
  { name :: String
  , appliesTo :: LintContext -> a -> Boolean
  }

-- | A named reason no rule should look at a module at all.
type ModuleExemption =
  { name :: String
  , appliesTo :: LintContext -> Boolean
  }

-- | A check plus the exemptions guarding it.
type Guarded a =
  { exemptions :: Array (LintExemption a), check :: LintContext -> a -> LintResult a }

-- | Run a check unless an exemption applies, in which case pass.
skipWhen :: ∀ a. Guarded a -> LintContext -> a -> LintResult a
skipWhen { exemptions, check } context value =
  Maybe.maybe (check context value) (const Passed)
    (Array.find (\exemption -> exemption.appliesTo context value) exemptions)

-- | Options common to a rule at any level.
class RuleOptions r where
  -- | Turn a rule off while leaving it in the set, so the line stays
  -- | visible.
  disabled :: Boolean -> r -> r

-- | Attaching exemptions, whose type depends on what the rule sees.
class HasExclude r a | r -> a where
  -- | Give a rule reasons to skip particular values.
  exclude :: Array (LintExemption a) -> r -> r

-- | What the runner needs of a rule, whatever level it checks.
class RuleLike r a | r -> a where
  -- | Whether this rule was switched off.
  ruleDisabled :: r -> Boolean
  -- | The reasons this rule skips particular values.
  ruleExclude :: r -> Array (LintExemption a)
  -- | The check itself.
  ruleCheck :: r -> LintContext -> a -> LintResult a
  -- | How the rule describes itself, for the report.
  ruleInfo :: r -> RuleInfo

-- | A module rule with its options applied. The setting is gone by
-- | here: `perModule` handed it to the check and printed it for the
-- | report, so a set can hold rules configured every which way.
newtype ModuleRule = ModuleRule
  { exclude :: Array (LintExemption (CST.Module Void))
  , disabled :: Boolean
  , check :: LintContext -> CST.Module Void -> LintResult (CST.Module Void)
  , info :: RuleInfo
  }

-- | Run this rule once per module, at this setting.
perModule :: ∀ cfg. ModuleLint cfg -> cfg -> ModuleRule
perModule lint config = ModuleRule
  { exclude: []
  , disabled: false
  , check: lint.rule config
  , info: ruleInfoOf lint
  }

-- | Run a rule that has nothing to configure once per module.
perModule_ :: ModuleLint Unit -> ModuleRule
perModule_ lint = perModule lint unit

instance RuleOptions ModuleRule where
  disabled d (ModuleRule r) = ModuleRule (r { disabled = d })

instance HasExclude ModuleRule (CST.Module Void) where
  exclude ex (ModuleRule r) = ModuleRule (r { exclude = ex })

instance RuleLike ModuleRule (CST.Module Void) where
  ruleDisabled (ModuleRule r) = r.disabled
  ruleExclude (ModuleRule r) = r.exclude
  ruleCheck (ModuleRule r) = r.check
  ruleInfo (ModuleRule r) = r.info

-- | A declaration rule with its options applied.
newtype DeclarationRule = DeclarationRule
  { exclude :: Array (LintExemption (CST.Declaration Void))
  , disabled :: Boolean
  , check :: LintContext -> CST.Declaration Void -> LintResult (CST.Declaration Void)
  , info :: RuleInfo
  }

-- | Run this rule once per top-level declaration, at this setting.
perDecl :: ∀ cfg. DeclarationLint cfg -> cfg -> DeclarationRule
perDecl lint config = DeclarationRule
  { exclude: []
  , disabled: false
  , check: lint.rule config
  , info: ruleInfoOf lint
  }

-- | Run a rule that has nothing to configure once per declaration.
perDecl_ :: DeclarationLint Unit -> DeclarationRule
perDecl_ lint = perDecl lint unit

instance RuleOptions DeclarationRule where
  disabled d (DeclarationRule r) = DeclarationRule (r { disabled = d })

instance HasExclude DeclarationRule (CST.Declaration Void) where
  exclude ex (DeclarationRule r) = DeclarationRule (r { exclude = ex })

instance RuleLike DeclarationRule (CST.Declaration Void) where
  ruleDisabled (DeclarationRule r) = r.disabled
  ruleExclude (DeclarationRule r) = r.exclude
  ruleCheck (DeclarationRule r) = r.check
  ruleInfo (DeclarationRule r) = r.info

-- | An expression rule with its options applied.
newtype ExprRule = ExprRule
  { exclude :: Array (LintExemption (CST.Expr Void))
  , disabled :: Boolean
  , check :: LintContext -> CST.Expr Void -> LintResult (CST.Expr Void)
  , info :: RuleInfo
  }

-- | Run this rule over every expression, bottom-up, so a rewrite is
-- | visible to the rules that see the enclosing expression.
perExpr :: ∀ cfg. ExprLint cfg -> cfg -> ExprRule
perExpr lint config = ExprRule
  { exclude: []
  , disabled: false
  , check: lint.rule config
  , info: ruleInfoOf lint
  }

-- | Run a rule that has nothing to configure over every expression.
perExpr_ :: ExprLint Unit -> ExprRule
perExpr_ lint = perExpr lint unit

instance RuleOptions ExprRule where
  disabled d (ExprRule r) = ExprRule (r { disabled = d })

instance HasExclude ExprRule (CST.Expr Void) where
  exclude ex (ExprRule r) = ExprRule (r { exclude = ex })

instance RuleLike ExprRule (CST.Expr Void) where
  ruleDisabled (ExprRule r) = r.disabled
  ruleExclude (ExprRule r) = r.exclude
  ruleCheck (ExprRule r) = r.check
  ruleInfo (ExprRule r) = r.info

-- | Everything a report needs of a rule, taken while its setting is
-- | still in scope.
ruleInfoOf
  :: ∀ cfg r
   . { name :: String
     , description :: String
     , examples :: Maybe (Examples cfg)
     | r
     }
  -> RuleInfo
ruleInfoOf lint =
  { name: lint.name
  , description: lint.description
  , examples: map printed lint.examples
  }
  where
  printed examples =
    { config: examples.printConfig examples.config
    , good: examples.good
    , bad: examples.bad
    }

-- | A rule's own description of itself, carried alongside anything it
-- | finds so the report can say which rule spoke and what it wants.
type RuleInfo =
  { name :: String
  , description :: String
  , examples :: Maybe
      { config :: Maybe String
      , good :: Array String
      , bad :: Array String
      }
  }

-- | A rule, and the groups it was written under.
type Grouped a = { groups :: Array String, rule :: a }

-- | One thing a rule found, which rule found it, and where that rule
-- | sits in the set.
type Finding =
  { rule :: RuleInfo
  , groups :: Array String
  , message :: String
  , hint :: Maybe String
  }

-- | What running rules over one value produced: the value (rewritten if
-- | any rule fixed it), whether that happened, and everything found.
type RuleOutcome a = { result :: a, fixed :: Boolean, violations :: Array Finding }

-- | Run every rule over one value, threading each rewrite into the next
-- | rule's input.
runRules
  :: ∀ r a. RuleLike r a => LintContext -> Array (Grouped r) -> a -> RuleOutcome a
runRules context rules initial =
  let
    applyOne acc { groups, rule: r }
      | ruleDisabled r = acc
      | otherwise =
          case skipWhen { exemptions: ruleExclude r, check: ruleCheck r } context acc.result of
            Passed -> acc
            Violations found hint -> acc
              { violations =
                  acc.violations <> map (asFinding groups r hint) (NEA.toArray found)
              }
            Fixed result -> acc { result = result, fixed = true }
    asFinding groups r hint message = { rule: ruleInfo r, groups, message, hint }
  in
    Array.foldl applyOne { result: initial, fixed: false, violations: [] } rules
