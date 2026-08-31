module Lint.Internal.Survey
  ( PackageLint
  , PackageRule
  , PackageSurvey
  , Subject(..)
  , SubjectExemption
  , SurveyFinding
  , SurveyModule
  , WorkspaceLint
  , WorkspaceRule
  , WorkspaceSurvey
  , class HasSubjectIgnore
  , class SurveyLike
  , ignoreSubjects
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
import Lint.Internal.Rule (class RuleOptions, Grouped, ModuleKind)

-- | One module, as a survey sees it: where it is, not what is in it.
type SurveyModule = { moduleName :: String, path :: FilePath, kind :: ModuleKind }

-- | What a survey rule is complaining about.
data Subject
  = Namespace String
  | Package String
  | Module String

derive instance eqSubject :: Eq Subject

instance showSubject :: Show Subject where
  show = case _ of
    Namespace n -> n
    Package n -> n
    Module n -> n

-- | One finding, and what it is about.
type SurveyFinding = { subject :: Subject, message :: String }

-- | A named reason a survey rule's findings about some subject should
-- | be dropped. Unlike `exclude`, which stops a rule seeing a value at
-- | all, this runs after the rule has looked: a survey rule is handed
-- | the whole workspace at once, so its findings can only be filtered
-- | once made.
type SubjectExemption = { name :: String, appliesTo :: Subject -> Boolean }

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
  , goodExamples :: Array String
  , badExamples :: Array String
  , exampleConfig :: Maybe String
  , rule :: PackageSurvey -> Array SurveyFinding
  }

-- | A rule that sees every package's layout at once.
type WorkspaceLint =
  { name :: String
  , description :: String
  , goodExamples :: Array String
  , badExamples :: Array String
  , exampleConfig :: Maybe String
  , rule :: WorkspaceSurvey -> Array SurveyFinding
  }

-- | A package rule with its options applied.
newtype PackageRule = PackageRule
  { exclude :: Array SubjectExemption
  , disabled :: Boolean
  , rule :: PackageLint
  }

-- | Run this rule once per package.
perPackage :: PackageLint -> PackageRule
perPackage check =
  PackageRule { exclude: [], disabled: false, rule: check }

instance RuleOptions PackageRule where
  disabled d (PackageRule r) = PackageRule (r { disabled = d })

instance HasSubjectIgnore PackageRule where
  ignoreSubjects ex (PackageRule r) = PackageRule (r { exclude = ex })

-- | A workspace rule with its options applied.
newtype WorkspaceRule = WorkspaceRule
  { exclude :: Array SubjectExemption
  , disabled :: Boolean
  , rule :: WorkspaceLint
  }

-- | Run this rule once over the whole workspace.
perWorkspace :: WorkspaceLint -> WorkspaceRule
perWorkspace check =
  WorkspaceRule { exclude: [], disabled: false, rule: check }

instance RuleOptions WorkspaceRule where
  disabled d (WorkspaceRule r) = WorkspaceRule (r { disabled = d })

instance HasSubjectIgnore WorkspaceRule where
  ignoreSubjects ex (WorkspaceRule r) = WorkspaceRule (r { exclude = ex })

-- | Attaching exemptions to a survey rule.
class HasSubjectIgnore r where
  -- | Give a survey rule reasons to drop findings about particular
-- | subjects, after it has made them.
  ignoreSubjects :: Array SubjectExemption -> r -> r

-- | What the runner needs of a survey rule, at either scope.
class SurveyLike r s | r -> s where
  -- | Whether this rule was switched off.
  surveyDisabled :: r -> Boolean
  -- | The reasons this rule ignores particular findings.
  surveyExclude :: r -> Array SubjectExemption
  -- | The check itself.
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
runSurveyRules
  :: ∀ r s. SurveyLike r s => Array (Grouped r) -> s -> Array String
runSurveyRules rules survey =
  let
    applyOne { rule: r }
      | surveyDisabled r = []
      | otherwise = map _.message (Array.filter (kept r) (surveyCheck r survey))
  in
    Array.concatMap applyOne rules

kept :: ∀ r s. SurveyLike r s => r -> SurveyFinding -> Boolean
kept r finding = not (Array.any (\ex -> ex.appliesTo finding.subject) (surveyExclude r))
