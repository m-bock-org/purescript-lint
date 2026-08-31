module Lint.Internal.Rule
  ( Guarded
  , DeclarationLint
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
  , dedent
  , disabled
  , exclude
  , fixed
  , perDecl
  , perExpr
  , perModule
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

-- | A rule that sees one whole module.
type ModuleLint =
  { name :: String
  , description :: String
  , goodExamples :: Array String
  , badExamples :: Array String
  , exampleConfig :: Maybe String
  , rule :: LintContext -> CST.Module Void -> LintResult (CST.Module Void)
  }

-- | A rule that sees one top-level declaration.
type DeclarationLint =
  { name :: String
  , description :: String
  , goodExamples :: Array String
  , badExamples :: Array String
  , exampleConfig :: Maybe String
  , rule :: LintContext -> CST.Declaration Void -> LintResult (CST.Declaration Void)
  }

-- | A rule that sees one expression, anywhere inside a declaration.
type ExprLint =
  { name :: String
  , description :: String
  , goodExamples :: Array String
  , badExamples :: Array String
  , exampleConfig :: Maybe String
  , rule :: LintContext -> CST.Expr Void -> LintResult (CST.Expr Void)
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

-- | Strip a leading and trailing blank line and the shared indentation
-- | from the rest, so an example can be written indented to match
-- | the code around it.
dedent :: String -> String
dedent raw =
  let
    allLines = Str.split (Str.Pattern "\n") raw

    withoutLeadingBlank = case Array.head allLines, Array.tail allLines of
      Just first, Just rest | Str.trim first == "" -> rest
      _, _ -> allLines

    contentLines = case Array.init withoutLeadingBlank, Array.last withoutLeadingBlank of
      Just initLines, Just lastLine | Str.trim lastLine == "" -> initLines
      _, _ -> withoutLeadingBlank

    indentOf line = Str.length line - Str.length (Str.dropWhile (_ == ' ') line)

    nonBlank = Array.filter (\l -> Str.trim l /= "") contentLines
    commonIndent = Maybe.fromMaybe 0 (Foldable.minimum (map indentOf nonBlank))

    stripIndent line
      | Str.trim line == "" = ""
      | otherwise = Str.drop commonIndent line
  in
    Str.joinWith "\n" (map stripIndent contentLines)

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

-- | A module rule with its options applied.
newtype ModuleRule = ModuleRule
  { exclude :: Array (LintExemption (CST.Module Void))
  , disabled :: Boolean
  , rule :: ModuleLint
  }

-- | Run this rule once per module.
perModule :: ModuleLint -> ModuleRule
perModule check =
  ModuleRule { exclude: [], disabled: false, rule: check }

instance RuleOptions ModuleRule where
  disabled d (ModuleRule r) = ModuleRule (r { disabled = d })

instance HasExclude ModuleRule (CST.Module Void) where
  exclude ex (ModuleRule r) = ModuleRule (r { exclude = ex })

instance RuleLike ModuleRule (CST.Module Void) where
  ruleDisabled (ModuleRule r) = r.disabled
  ruleExclude (ModuleRule r) = r.exclude
  ruleCheck (ModuleRule r) = r.rule.rule
  ruleInfo (ModuleRule r) =
    { name: r.rule.name
    , description: r.rule.description
    , goodExamples: r.rule.goodExamples
    , badExamples: r.rule.badExamples
    , exampleConfig: r.rule.exampleConfig
    }

-- | A declaration rule with its options applied.
newtype DeclarationRule = DeclarationRule
  { exclude :: Array (LintExemption (CST.Declaration Void))
  , disabled :: Boolean
  , rule :: DeclarationLint
  }

-- | Run this rule once per top-level declaration.
perDecl :: DeclarationLint -> DeclarationRule
perDecl check =
  DeclarationRule { exclude: [], disabled: false, rule: check }

instance RuleOptions DeclarationRule where
  disabled d (DeclarationRule r) = DeclarationRule (r { disabled = d })

instance HasExclude DeclarationRule (CST.Declaration Void) where
  exclude ex (DeclarationRule r) = DeclarationRule (r { exclude = ex })

instance RuleLike DeclarationRule (CST.Declaration Void) where
  ruleDisabled (DeclarationRule r) = r.disabled
  ruleExclude (DeclarationRule r) = r.exclude
  ruleCheck (DeclarationRule r) = r.rule.rule
  ruleInfo (DeclarationRule r) =
    { name: r.rule.name
    , description: r.rule.description
    , goodExamples: r.rule.goodExamples
    , badExamples: r.rule.badExamples
    , exampleConfig: r.rule.exampleConfig
    }

-- | An expression rule with its options applied.
newtype ExprRule = ExprRule
  { exclude :: Array (LintExemption (CST.Expr Void))
  , disabled :: Boolean
  , rule :: ExprLint
  }

-- | Run this rule over every expression, bottom-up, so a rewrite is
-- | visible to the rules that see the enclosing expression.
perExpr :: ExprLint -> ExprRule
perExpr check =
  ExprRule { exclude: [], disabled: false, rule: check }

instance RuleOptions ExprRule where
  disabled d (ExprRule r) = ExprRule (r { disabled = d })

instance HasExclude ExprRule (CST.Expr Void) where
  exclude ex (ExprRule r) = ExprRule (r { exclude = ex })

instance RuleLike ExprRule (CST.Expr Void) where
  ruleDisabled (ExprRule r) = r.disabled
  ruleExclude (ExprRule r) = r.exclude
  ruleCheck (ExprRule r) = r.rule.rule
  ruleInfo (ExprRule r) =
    { name: r.rule.name
    , description: r.rule.description
    , goodExamples: r.rule.goodExamples
    , badExamples: r.rule.badExamples
    , exampleConfig: r.rule.exampleConfig
    }

-- | A rule's own description of itself, carried alongside anything it
-- | finds so the report can say which rule spoke and what it wants.
type RuleInfo =
  { name :: String
  , description :: String
  , goodExamples :: Array String
  , badExamples :: Array String
  , exampleConfig :: Maybe String
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
