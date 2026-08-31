module PureScript.Lint.Rule
  ( DeclarationLint
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
  , RuleOutcome
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
  , violation
  , violations
  , withHint
  ) where

import Prelude

import Data.Array (filter, find, foldl, head, init, last, tail) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (fromArray, singleton, toArray) as NEA
import Data.Foldable (minimum) as Foldable
import Data.Maybe (Maybe(..))
import Data.Maybe (fromMaybe, maybe) as Maybe
import Data.String (Pattern(..)) as Str
import Data.String.CodeUnits (drop, dropWhile, length) as Str
import Data.String.Common (joinWith, split, trim) as Str
import Node.Path (FilePath)
import PureScript.CST.Types (Declaration, Expr, Module) as CST

data LintResult a
  = Passed
  | Violations (NonEmptyArray String) (Maybe String)
  | Fixed a

-- | One finding.
violation :: ∀ a. String -> LintResult a
violation message = Violations (NEA.singleton message) Nothing

-- | Every finding a rule made. No findings is how a rule passes, so
-- | there is no separate `passed` to reach for - and a rule that
-- | computes its findings does not have to special-case the empty run.
violations :: ∀ a. Array String -> LintResult a
violations found =
  Maybe.maybe Passed (\vs -> Violations vs Nothing) (NEA.fromArray found)

-- | A rule that rewrote what it was given, rather than complaining
-- | about it.
fixed :: ∀ a. a -> LintResult a
fixed = Fixed

-- | Attach a suggestion to every finding of a result. A hint belongs to
-- | the rule rather than to any one finding - the rules that carry one
-- | say the same thing however many things they found - so it is set
-- | here rather than passed in alongside each message.
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

type LintContext =
  { packageName :: String
  , moduleName :: String
  , declarationName :: Maybe String
  , path :: FilePath
  , kind :: ModuleKind
  }

type ModuleLint =
  { name :: String
  , description :: String
  , goodExample :: Maybe String
  , badExample :: Maybe String
  , rule :: LintContext -> CST.Module Void -> LintResult (CST.Module Void)
  }

type DeclarationLint =
  { name :: String
  , description :: String
  , goodExample :: Maybe String
  , badExample :: Maybe String
  , rule :: LintContext -> CST.Declaration Void -> LintResult (CST.Declaration Void)
  }

type ExprLint =
  { name :: String
  , description :: String
  , goodExample :: Maybe String
  , badExample :: Maybe String
  , rule :: LintContext -> CST.Expr Void -> LintResult (CST.Expr Void)
  }

type LintExemption a =
  { name :: String
  , appliesTo :: LintContext -> a -> Boolean
  }

type ModuleExemption =
  { name :: String
  , appliesTo :: LintContext -> Boolean
  }

type Guarded a =
  { exemptions :: Array (LintExemption a), check :: LintContext -> a -> LintResult a }

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

class HasExclude r a | r -> a where
  exclude :: Array (LintExemption a) -> r -> r

class RuleLike r a | r -> a where
  ruleDisabled :: r -> Boolean
  ruleExclude :: r -> Array (LintExemption a)
  ruleCheck :: r -> LintContext -> a -> LintResult a

newtype ModuleRule = ModuleRule
  { exclude :: Array (LintExemption (CST.Module Void))
  , disabled :: Boolean
  , rule :: ModuleLint
  }

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

newtype DeclarationRule = DeclarationRule
  { exclude :: Array (LintExemption (CST.Declaration Void))
  , disabled :: Boolean
  , rule :: DeclarationLint
  }

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

newtype ExprRule = ExprRule
  { exclude :: Array (LintExemption (CST.Expr Void))
  , disabled :: Boolean
  , rule :: ExprLint
  }

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

type RuleOutcome a = { result :: a, fixed :: Boolean, violations :: Array String }

runRules :: ∀ r a. RuleLike r a => LintContext -> Array r -> a -> RuleOutcome a
runRules context rules initial =
  let
    applyOne acc r
      | ruleDisabled r = acc
      | otherwise =
          case skipWhen { exemptions: ruleExclude r, check: ruleCheck r } context acc.result of
            Passed -> acc
            Violations found hint -> acc
              { violations = acc.violations <> map (withSuffix hint) (NEA.toArray found) }
            Fixed result -> acc { result = result, fixed = true }
    withSuffix hint msg = case hint of
      Just h -> msg <> " (hint: " <> h <> ")"
      Nothing -> msg
  in
    Array.foldl applyOne { result: initial, fixed: false, violations: [] } rules
