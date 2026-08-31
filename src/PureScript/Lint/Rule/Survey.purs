module PureScript.Lint.Rule.Survey
  ( PackageLint
  , PackageRule
  , PackageSurvey
  , PageExemption
  , SurveyFinding
  , SurveyModule
  , WorkspaceLint
  , WorkspaceRule
  , WorkspaceSurvey
  , class HasPageExclude
  , class SurveyLike
  , excludePages
  , perPackage
  , perWorkspace
  , runSurveyRules
  , surveyCheck
  , surveyDisabled
  , surveyExclude
  ) where

import Prelude

import Data.Array (any, concatMap, filter) as Array
import Data.Maybe (Maybe)
import Node.Path (FilePath)
import PureScript.Lint.Rule (class RuleOptions, RuleName)
import PureScript.Lint.Workspace (ModuleKind)

type SurveyModule = { moduleName :: String, path :: FilePath, kind :: ModuleKind }

type SurveyFinding = { page :: String, message :: String }

type PageExemption = { name :: String, appliesTo :: String -> Boolean }

type PackageSurvey =
  { packageName :: String
  , packagePath :: FilePath
  , modules :: Array SurveyModule
  }

type WorkspaceSurvey = { packages :: Array PackageSurvey }

type PackageLint =
  { name :: RuleName
  , description :: String
  , goodExample :: Maybe String
  , badExample :: Maybe String
  , rule :: PackageSurvey -> Array SurveyFinding
  }

type WorkspaceLint =
  { name :: RuleName
  , description :: String
  , goodExample :: Maybe String
  , badExample :: Maybe String
  , rule :: WorkspaceSurvey -> Array SurveyFinding
  }

newtype PackageRule = PackageRule
  { exclude :: Array PageExemption
  , disabled :: Boolean
  , rule :: PackageLint
  }

perPackage :: PackageLint -> PackageRule
perPackage check =
  PackageRule { exclude: [], disabled: false, rule: check }

instance RuleOptions PackageRule where
  disabled d (PackageRule r) = PackageRule (r { disabled = d })

instance HasPageExclude PackageRule where
  excludePages ex (PackageRule r) = PackageRule (r { exclude = ex })

newtype WorkspaceRule = WorkspaceRule
  { exclude :: Array PageExemption
  , disabled :: Boolean
  , rule :: WorkspaceLint
  }

perWorkspace :: WorkspaceLint -> WorkspaceRule
perWorkspace check =
  WorkspaceRule { exclude: [], disabled: false, rule: check }

instance RuleOptions WorkspaceRule where
  disabled d (WorkspaceRule r) = WorkspaceRule (r { disabled = d })

instance HasPageExclude WorkspaceRule where
  excludePages ex (WorkspaceRule r) = WorkspaceRule (r { exclude = ex })

class HasPageExclude r where
  excludePages :: Array PageExemption -> r -> r

class SurveyLike r s | r -> s where
  surveyDisabled :: r -> Boolean
  surveyExclude :: r -> Array PageExemption
  surveyCheck :: r -> s -> Array SurveyFinding

instance SurveyLike PackageRule PackageSurvey where
  surveyDisabled (PackageRule r) = r.disabled
  surveyExclude (PackageRule r) = r.exclude
  surveyCheck (PackageRule r) = r.rule.rule

instance SurveyLike WorkspaceRule WorkspaceSurvey where
  surveyDisabled (WorkspaceRule r) = r.disabled
  surveyExclude (WorkspaceRule r) = r.exclude
  surveyCheck (WorkspaceRule r) = r.rule.rule

-- | Uses `kept`.
runSurveyRules :: ∀ r s. SurveyLike r s => Array r -> s -> Array String
runSurveyRules rules survey =
  let
    applyOne r
      | surveyDisabled r = []
      | otherwise = map _.message (Array.filter (kept r) (surveyCheck r survey))
  in
    Array.concatMap applyOne rules

-- | Private. Used only by `runSurveyRules`.
kept :: ∀ r s. SurveyLike r s => r -> SurveyFinding -> Boolean
kept r finding = not (Array.any (\ex -> ex.appliesTo finding.page) (surveyExclude r))

-- ## Context
--
-- Rules that look at a whole package, or the whole workspace, rather
-- than at one CST node. Split out of `PureScript.Lint.Rule` (2026-08-29) when
-- adding them pushed that module past its own declaration and export
-- limits - which is the answer `max-modules-per-namespace` exists to
-- force, applied to the module that introduced it.
--
-- A survey rule returns `Array SurveyFinding` rather than
-- `LintResult a`, and the missing case is `Fixed`. That is deliberate,
-- not an omission waiting to be filled: there is no single value to
-- rewrite. Acting on one of these findings means moving files, renaming
-- modules and rewriting every import that named them - and *which*
-- things move is the entire content of the decision. A linter that
-- picked would be answering the question the rule exists to ask. The
-- runner has no way to apply a survey fix even if a rule offered one.
--
-- Findings carry the `page` they are about, separately from the message,
-- so the runner can drop the ones an exemption covers. The rule does not
-- do its own filtering: it reports what it found and where, and the
-- config decides what is currently tolerated - the same division every
-- CST-level rule already follows via `exclude`.
--
-- `PageExemption` matches on that page name (a namespace, or a
-- directory) rather than on a `LintContext`, because a survey rule has
-- no single module to describe. That is also why this needs its own
-- class instead of reusing `HasExclude`, whose exemption type is
-- indexed by the CST value being checked.
--
-- `PackageSurvey` carries both the package's name and its path. The two
-- do not have to agree - kebab-case names do not encode directory
-- nesting reliably - so a rule takes whichever one it actually means.
