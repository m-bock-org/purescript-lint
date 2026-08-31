module PureScript.Lint.Rule
  ( DeclarationLint
  , DeclarationRule
  , ExprLint
  , ExprRule
  , GlobalExemption
  , LintContext
  , LintExemption
  , LintResult(..)
  , ModuleLint
  , ModuleRule
  , RuleAlias(..)
  , RuleName(..)
  , RuleOutcome
  , class HasExclude
  , class RuleLike
  , class RuleOptions
  , dedent
  , disabled
  , exclude
  , name
  , perDecl
  , perExpr
  , perModule
  , ruleCheck
  , ruleDisabled
  , ruleExclude
  , ruleName
  , runRules
  , skipWhen
  ) where

import Prelude

import Data.Array (elem, filter, find, foldl, head, init, last, snoc, tail) as Array
import Data.Foldable (minimum) as Foldable
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe, maybe) as Maybe
import Data.String (Pattern(..)) as Str
import Data.String.CodeUnits (drop, dropWhile, length) as Str
import Data.String.Common (joinWith, split, trim) as Str
import Node.Path (FilePath)
import PureScript.CST.Types (Declaration, Expr, Module) as CST
import PureScript.Lint.Workspace (ModuleKind)

data LintResult a
  = Passed
  | Violation String
  | ViolationWithHint String String
  | Fixed a

newtype RuleName = RuleName String

newtype RuleAlias = RuleAlias String

derive newtype instance Eq RuleAlias

type LintContext =
  { packageName :: String
  , moduleName :: String
  , declarationName :: Maybe String
  , path :: FilePath
  , kind :: ModuleKind
  }

type ModuleLint =
  { name :: RuleName
  , description :: String
  , goodExample :: Maybe String
  , badExample :: Maybe String
  , rule :: LintContext -> CST.Module Void -> LintResult (CST.Module Void)
  }

type DeclarationLint =
  { name :: RuleName
  , description :: String
  , goodExample :: Maybe String
  , badExample :: Maybe String
  , rule :: LintContext -> CST.Declaration Void -> LintResult (CST.Declaration Void)
  }

type ExprLint =
  { name :: RuleName
  , description :: String
  , goodExample :: Maybe String
  , badExample :: Maybe String
  , rule :: LintContext -> CST.Expr Void -> LintResult (CST.Expr Void)
  }

type LintExemption a =
  { name :: String
  , appliesTo :: LintContext -> a -> Boolean
  }

type GlobalExemption =
  { name :: String
  , appliesTo :: LintContext -> Boolean
  }

type Guarded a =
  { exemptions :: Array (LintExemption a), check :: LintContext -> a -> LintResult a }

type Scope = { excludeRules :: Array RuleAlias, context :: LintContext }

skipWhen :: ∀ a. Guarded a -> LintContext -> a -> LintResult a
skipWhen { exemptions, check } context value =
  Maybe.maybe (check context value) (const Passed)
    (Array.find (\exemption -> exemption.appliesTo context value) exemptions)

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

class RuleOptions r where
  disabled :: Boolean -> r -> r
  name :: RuleAlias -> r -> r

class HasExclude r a | r -> a where
  exclude :: Array (LintExemption a) -> r -> r

class RuleLike r a | r -> a where
  ruleName :: r -> Maybe RuleAlias
  ruleDisabled :: r -> Boolean
  ruleExclude :: r -> Array (LintExemption a)
  ruleCheck :: r -> LintContext -> a -> LintResult a

newtype ModuleRule = ModuleRule
  { name :: Maybe RuleAlias
  , exclude :: Array (LintExemption (CST.Module Void))
  , disabled :: Boolean
  , rule :: ModuleLint
  }

perModule :: ModuleLint -> ModuleRule
perModule check =
  ModuleRule { name: Nothing, exclude: [], disabled: false, rule: check }

instance RuleOptions ModuleRule where
  disabled d (ModuleRule r) = ModuleRule (r { disabled = d })
  name n (ModuleRule r) = ModuleRule (r { name = Just n })

instance HasExclude ModuleRule (CST.Module Void) where
  exclude ex (ModuleRule r) = ModuleRule (r { exclude = ex })

instance RuleLike ModuleRule (CST.Module Void) where
  ruleName (ModuleRule r) = r.name
  ruleDisabled (ModuleRule r) = r.disabled
  ruleExclude (ModuleRule r) = r.exclude
  ruleCheck (ModuleRule r) = r.rule.rule

newtype DeclarationRule = DeclarationRule
  { name :: Maybe RuleAlias
  , exclude :: Array (LintExemption (CST.Declaration Void))
  , disabled :: Boolean
  , rule :: DeclarationLint
  }

perDecl :: DeclarationLint -> DeclarationRule
perDecl check =
  DeclarationRule { name: Nothing, exclude: [], disabled: false, rule: check }

instance RuleOptions DeclarationRule where
  disabled d (DeclarationRule r) = DeclarationRule (r { disabled = d })
  name n (DeclarationRule r) = DeclarationRule (r { name = Just n })

instance HasExclude DeclarationRule (CST.Declaration Void) where
  exclude ex (DeclarationRule r) = DeclarationRule (r { exclude = ex })

instance RuleLike DeclarationRule (CST.Declaration Void) where
  ruleName (DeclarationRule r) = r.name
  ruleDisabled (DeclarationRule r) = r.disabled
  ruleExclude (DeclarationRule r) = r.exclude
  ruleCheck (DeclarationRule r) = r.rule.rule

newtype ExprRule = ExprRule
  { name :: Maybe RuleAlias
  , exclude :: Array (LintExemption (CST.Expr Void))
  , disabled :: Boolean
  , rule :: ExprLint
  }

perExpr :: ExprLint -> ExprRule
perExpr check =
  ExprRule { name: Nothing, exclude: [], disabled: false, rule: check }

instance RuleOptions ExprRule where
  disabled d (ExprRule r) = ExprRule (r { disabled = d })
  name n (ExprRule r) = ExprRule (r { name = Just n })

instance HasExclude ExprRule (CST.Expr Void) where
  exclude ex (ExprRule r) = ExprRule (r { exclude = ex })

instance RuleLike ExprRule (CST.Expr Void) where
  ruleName (ExprRule r) = r.name
  ruleDisabled (ExprRule r) = r.disabled
  ruleExclude (ExprRule r) = r.exclude
  ruleCheck (ExprRule r) = r.rule.rule

type RuleOutcome a = { result :: a, fixed :: Boolean, violations :: Array String }

-- | Uses `skipWhen`.
runRules :: ∀ r a. RuleLike r a => Scope -> Array r -> a -> RuleOutcome a
runRules { excludeRules, context } rules initial =
  let
    applyOne acc r
      | ruleDisabled r = acc
      | Just n <- ruleName r, n `Array.elem` excludeRules = acc
      | otherwise =
          case skipWhen { exemptions: ruleExclude r, check: ruleCheck r } context acc.result of
            Passed -> acc
            Violation msg -> acc { violations = Array.snoc acc.violations msg }
            ViolationWithHint msg hint -> acc
              { violations = Array.snoc acc.violations (msg <> " (hint: " <> hint <> ")") }
            Fixed result -> acc { result = result, fixed = true }
  in
    Array.foldl applyOne { result: initial, fixed: false, violations: [] } rules

-- Context: one named lint rule, checked at up to three CST levels
-- (module, declaration, expression) - a rule doesn't have to check
-- every level, an empty array at a given level just means this rule
-- has nothing to say there.
--
-- LintResult's `Passed`/`Violation` mirror `Either`'s `Right`/`Left`;
-- `Fixed` is the case `Either` can't express on its own - the rule
-- both flagged *and* rewrote the value, so the caller gets the
-- correction back instead of just the complaint. `ViolationWithHint`
-- is `Violation` plus a separate suggestion for how to fix it by hand
-- (e.g. "use do notation instead") - kept as its own constructor
-- rather than folded into `Violation`'s own string so a rule that has
-- nothing more to suggest isn't forced to invent one. LintContext
-- carries everything about where a checked value came from that a
-- rule might need but can't read off the value itself - a
-- `CST.Module Void` knows nothing about its own file path or which
-- package it belongs to.
--
-- ModuleLint/DeclarationLint/ExprLint's `name :: RuleName`/`description`
-- identify the general rule itself (e.g. `RuleName "max-function-arity"`,
-- "flags a function with more than N arguments") - distinct from
-- `ModuleRule`'s own (optional) `name :: Maybe RuleAlias`, which is the
-- CLI-`--exclude` identifier for one *configured* instance of a general
-- rule in `Lint.purs` (there could be more than one, e.g. two
-- `maxFunctionArity` calls with different thresholds). Two separate
-- newtypes (2026-08-28), not one shared type, precisely because they
-- can genuinely diverge - wrapping both the same way would silently
-- reintroduce the confusion this split exists to prevent. Neither
-- carries a smart constructor - both are freely built (`RuleName "..."`/
-- `RuleAlias "..."`), the same reasoning `Runtime.Core.Log.Tag` already uses:
-- a rule/alias literal only ever gets written once, in place, at its
-- own definition or wiring site, so a validating constructor would add
-- ceremony with nothing real to guard against. `RuleAlias` alone
-- derives `Eq` - the one thing `runRules`' `--exclude` matching
-- actually needs; `RuleName` has no derived instances since nothing
-- reads it outside its own rule's definition today.
-- `goodExample`/`badExample` are real PureScript source as a string,
-- not `forall a. a` - that type has no total inhabitant (only bottom:
-- a crash, an infinite loop, or `unsafeCoerce`, which
-- `no-unsafe-escape` already bans here), so it can never actually
-- hold example code. A plain string is printable and still reads as
-- real syntax in the source; the compiler just doesn't check it.
-- `Maybe` because most rules don't have one yet - filled in
-- opportunistically, not required up front.
--
-- LintExemption is a named, reusable reason to skip a check entirely
-- for one value - `name` exists so an exemption reads as
-- documentation at the call site (`skipWhen [ scratchModule ] rule`)
-- instead of an anonymous predicate whose intent has to be re-derived
-- every place it's reused. GlobalExemption is the same idea, but for
-- `runLinter`'s `globalExclude` - a value nothing at any of the three
-- CST levels needs to inspect (`scratchModule`'s whole reason for
-- existing is "which file is this", answerable from `LintContext`
-- alone), so there's no `a` to be polymorphic over in the first
-- place. skipWhen wraps a check so any matching exemption
-- short-circuits it to `Passed`, without the check itself needing to
-- know exemptions are even a thing - works for `ModuleLint` and
-- `DeclarationLint` alike, since both are just
-- `LintContext -> a -> LintResult a`.
--
-- dedent strips a blank leading/trailing line (the newline right
-- after and before a triple-quoted string's own `"""` delimiters) and
-- the common leading indentation off every remaining line, so a
-- `goodExample`/`badExample` can be written indented to match the
-- surrounding source instead of starting at column 0.
--
-- RuleOptions' `disabled`/`name` work the same way, whichever of the
-- three rule kinds (`ModuleRule`/`DeclarationRule`/`ExprRule`) is on
-- the other end - one class shared by all three, rather than three
-- identically-shaped pairs of functions under three different names.
-- HasExclude's `exclude` has a type that depends on which CST level
-- `r` checks (a `ModuleRule`'s exemptions look at a whole `Module`,
-- an `ExprRule`'s at one `Expr`), hence its own class instead of
-- folding into `RuleOptions` - the functional dependency (`r`
-- determines `a`) is what lets `exclude [ somePendingList ]`
-- type-check without an annotation at every call site. RuleLike is
-- what `runRules` needs to fold over a rule generically, regardless
-- of which opaque type it actually is - each rule kind's own instance
-- is the only place that can implement this (the constructor is
-- opaque everywhere else), which is exactly why `runRules` itself
-- lives here instead of in `Linter`.
--
-- ModuleRule/DeclarationRule/ExprRule are opaque - the only way to
-- build one is `perModule`/`perDecl`/`perExpr`, the only way to adjust
-- one is `name`/`exclude`/`disabled`. No exported constructor means no
-- record-literal path in, so there's no way to build one with, say,
-- `disabled` and `rule` swapped by a typo in field order. Each smart
-- constructor builds from just the check itself - `name: Nothing`,
-- `exclude: []`, `disabled: false` until `name`/`exclude`/`disabled`
-- say otherwise, e.g. `perModule requireTupleOperator # exclude
-- [ pending ] # disabled true`.
--
-- runRules threads one value through a list of rules in order - works
-- for `ModuleRule`, `DeclarationRule`, and `ExprRule` alike via
-- `RuleLike`, rather than three copies of the same fold. Each rule
-- sees the previous rule's `Fixed` output; a `disabled` rule or one
-- whose `name` is in `excludeRules` is skipped outright; everything
-- else runs through `skipWhen`.
--
-- `runLinter`'s `globalExclude` runs once per module, before any rule
-- in `lintRules` even sees it - a file it matches (e.g. a scratch
-- module) is skipped at every level, rather than needing every
-- individual rule to remember to list the same exemption in its own
-- `exclude`.
--
-- Rule/ToRule/group (2026-08-28) let `Lint.purs` mix module-,
-- declaration-, and expression-level rules in one flat, freely
-- orderable/groupable `Array Rule`, instead of forcing every call site
-- to sort each rule into one of three separate arrays by hand.
-- `RuleModule`/`RuleDeclaration`/`RuleExpr` just wrap the three
-- existing opaque rule types unchanged - nothing about `ModuleRule`/
-- `DeclarationRule`/`ExprRule` themselves changed, only how a
-- a rule set holds them. `ToRule`'s `rule` is a typeclass-based
-- injector purely for call-site ergonomics (`rule $ perModule
-- explicitImports` reads the same regardless of which of the three
-- kinds `perModule` produces) - it carries no logic of its own beyond
-- picking the matching constructor. `RuleGroup`'s `String` is
-- currently decorative only: a name to hang organization on at the
-- `Lint.purs` call site (`group "naming" [ ... ]`), not read by
-- anything at runtime today - `flattenRules` walks straight through
-- it. Nothing stops giving it a real runtime meaning later (e.g.
-- logging which group is running); that's just not wired up yet.
--
-- flattenRules turns an authored `Array Rule` (arbitrarily nested via
-- `RuleGroup`) into the three homogeneous arrays `runLinter` actually
-- runs, one bucket per rule kind, each in the order its rules appear
-- in a left-to-right, depth-first walk of the tree. This is a pure
-- reshaping step, not a behavior change - `Linter` still runs all
-- module rules, then all declaration rules, then all expression rules,
-- in that fixed sequence, exactly as it did when a record held
-- three separate arrays directly. Authoring order across *different*
-- kinds was never meaningful (a module rule earlier in the old
-- `lintModules` array vs. a declaration rule earlier in the old
-- `lintDeclarations` array had no relative ordering to begin with,
-- since the two arrays ran as separate passes) - only order *within*
-- one kind was ever meaningful, and `flattenRules` preserves that.
