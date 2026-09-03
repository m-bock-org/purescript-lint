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
  , perPackage_
  , perWorkspace
  , perWorkspace_
  , runSurveyRules
  , surveyCheck
  , surveyDisabled
  , surveyExclude
  , surveyName
  ) where

import Prelude

import Data.Array (any, concatMap, filter) as Array
import Node.Path (FilePath)
import Data.Maybe (Maybe(..))
import Lint.Internal.Exemptions (Exemptions)
import Lint.Internal.Exemptions as Exemptions
import Lint.Internal.Rule (class RuleOptions, Examples, Grouped, ModuleKind)

-- | One module, as a survey sees it: where it is, not what is in it.
type SurveyModule =
  { moduleName :: String
  , path :: FilePath
  , kind :: ModuleKind
  , imports :: Array String
  }

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
  , dependencies :: Array String
  , modules :: Array SurveyModule
  }

-- | Every package, for a rule that reasons across package boundaries.
type WorkspaceSurvey = { packages :: Array PackageSurvey }

-- | A rule that sees one package's layout.
type PackageLint cfg =
  { name :: String
  , description :: String
  , examples :: Maybe (Examples cfg)
  , rule :: cfg -> PackageSurvey -> Array SurveyFinding
  }

-- | A rule that sees every package's layout at once.
type WorkspaceLint cfg =
  { name :: String
  , description :: String
  , examples :: Maybe (Examples cfg)
  , rule :: cfg -> WorkspaceSurvey -> Array SurveyFinding
  }

-- | A package rule with its options applied.
newtype PackageRule = PackageRule
  { name :: String
  , exclude :: Array SubjectExemption
  , disabled :: Boolean
  , check :: PackageSurvey -> Array SurveyFinding
  }

-- | Run this rule once per package.
perPackage :: ∀ cfg. PackageLint cfg -> cfg -> PackageRule
perPackage lint config =
  PackageRule { name: lint.name, exclude: [], disabled: false, check: lint.rule config }

-- | Run a rule that has nothing to configure once per package.
perPackage_ :: PackageLint Unit -> PackageRule
perPackage_ lint = perPackage lint unit

instance RuleOptions PackageRule where
  disabled d (PackageRule r) = PackageRule (r { disabled = d })

instance HasSubjectIgnore PackageRule where
  ignoreSubjects ex (PackageRule r) = PackageRule (r { exclude = ex })

-- | A workspace rule with its options applied.
newtype WorkspaceRule = WorkspaceRule
  { name :: String
  , exclude :: Array SubjectExemption
  , disabled :: Boolean
  , check :: WorkspaceSurvey -> Array SurveyFinding
  }

-- | Run this rule once over the whole workspace.
perWorkspace :: ∀ cfg. WorkspaceLint cfg -> cfg -> WorkspaceRule
perWorkspace lint config =
  WorkspaceRule { name: lint.name, exclude: [], disabled: false, check: lint.rule config }

-- | Run a rule that has nothing to configure over the whole workspace.
perWorkspace_ :: WorkspaceLint Unit -> WorkspaceRule
perWorkspace_ lint = perWorkspace lint unit

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
  -- | What the rule is called.
  -- |
  -- | Kept because a finding has to be exemptable by rule name, and a
  -- | survey rule used to drop its name on the way in - which is also
  -- | why survey findings print as bare lines rather than grouped under
  -- | the rule that made them.
  surveyName :: r -> String

instance SurveyLike PackageRule PackageSurvey where
  surveyDisabled (PackageRule r) = r.disabled
  surveyExclude (PackageRule r) = r.exclude
  surveyCheck (PackageRule r) = r.check
  surveyName (PackageRule r) = r.name

instance SurveyLike WorkspaceRule WorkspaceSurvey where
  surveyDisabled (WorkspaceRule r) = r.disabled
  surveyExclude (WorkspaceRule r) = r.exclude
  surveyCheck (WorkspaceRule r) = r.check
  surveyName (WorkspaceRule r) = r.name

-- | Run every survey rule and collect what they found.
runSurveyRules
  :: ∀ r s. SurveyLike r s => Exemptions -> Array (Grouped r) -> s -> Array String
runSurveyRules exemptions rules survey =
  let
    applyOne { rule: r }
      | surveyDisabled r = []
      | otherwise = map _.message
          (Array.filter (\f -> kept r f && notExempt exemptions r f) (surveyCheck r survey))
  in
    Array.concatMap applyOne rules

kept :: ∀ r s. SurveyLike r s => r -> SurveyFinding -> Boolean
kept r finding = not (Array.any (\ex -> ex.appliesTo finding.subject) (surveyExclude r))

-- | Private. Used only by `runSurveyRules`. Uses `subjectName`.
-- |
-- | A survey rule sees the whole workspace, so its findings are about a
-- | module, package or namespace rather than about a place in a file -
-- | which is why they are filtered after the rule has looked rather
-- | than skipped before it. The file matches on the subject's name, so
-- | a module named there is exempt whichever kind of rule found it.
notExempt :: ∀ r s. SurveyLike r s => Exemptions -> r -> SurveyFinding -> Boolean
notExempt exemptions r finding = not
  ( Exemptions.matches exemptions
      { rule: surveyName r
      , moduleName: subjectName finding.subject
      , path: ""
      , declarationName: Nothing
      }
  )

-- | Private. Used only by `notExempt`.
subjectName :: Subject -> String
subjectName = case _ of
  Namespace name -> name
  Package name -> name
  Module name -> name
