module PureScript.Lint.Internal.Survey
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
import PureScript.Lint.Internal.Rule (class RuleOptions, ModuleKind)

-- | One module, as a survey sees it: where it is, not what is in it.
type SurveyModule = { moduleName :: String, path :: FilePath, kind :: ModuleKind }

-- | One finding, and which page of the survey it is about.
type SurveyFinding = { page :: String, message :: String }

-- | A named reason a survey rule should ignore one of its findings.
type PageExemption = { name :: String, appliesTo :: String -> Boolean }

-- | One package's modules, for a rule that reasons about a package.
type PackageSurvey =
  { packageName :: String
  , packagePath :: FilePath
  , modules :: Array SurveyModule
  }

-- | Every package, for a rule that reasons across package boundaries.
type WorkspaceSurvey = { packages :: Array PackageSurvey }

-- | A rule that sees one package's layout.
type PackageLint =
  { name :: String
  , description :: String
  , goodExample :: Maybe String
  , badExample :: Maybe String
  , rule :: PackageSurvey -> Array SurveyFinding
  }

-- | A rule that sees every package's layout at once.
type WorkspaceLint =
  { name :: String
  , description :: String
  , goodExample :: Maybe String
  , badExample :: Maybe String
  , rule :: WorkspaceSurvey -> Array SurveyFinding
  }

-- | A package rule with its options applied.
newtype PackageRule = PackageRule
  { exclude :: Array PageExemption
  , disabled :: Boolean
  , rule :: PackageLint
  }

-- | Run this rule once per package.
perPackage :: PackageLint -> PackageRule
perPackage check =
  PackageRule { exclude: [], disabled: false, rule: check }

instance RuleOptions PackageRule where
  disabled d (PackageRule r) = PackageRule (r { disabled = d })

instance HasPageExclude PackageRule where
  excludePages ex (PackageRule r) = PackageRule (r { exclude = ex })

-- | A workspace rule with its options applied.
newtype WorkspaceRule = WorkspaceRule
  { exclude :: Array PageExemption
  , disabled :: Boolean
  , rule :: WorkspaceLint
  }

-- | Run this rule once over the whole workspace.
perWorkspace :: WorkspaceLint -> WorkspaceRule
perWorkspace check =
  WorkspaceRule { exclude: [], disabled: false, rule: check }

instance RuleOptions WorkspaceRule where
  disabled d (WorkspaceRule r) = WorkspaceRule (r { disabled = d })

instance HasPageExclude WorkspaceRule where
  excludePages ex (WorkspaceRule r) = WorkspaceRule (r { exclude = ex })

-- | Attaching exemptions to a survey rule.
class HasPageExclude r where
  excludePages :: Array PageExemption -> r -> r

-- | What the runner needs of a survey rule, at either scope.
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

-- | Run every survey rule and collect what they found.
runSurveyRules :: ∀ r s. SurveyLike r s => Array r -> s -> Array String
runSurveyRules rules survey =
  let
    applyOne r
      | surveyDisabled r = []
      | otherwise = map _.message (Array.filter (kept r) (surveyCheck r survey))
  in
    Array.concatMap applyOne rules

kept :: ∀ r s. SurveyLike r s => r -> SurveyFinding -> Boolean
kept r finding = not (Array.any (\ex -> ex.appliesTo finding.page) (surveyExclude r))
