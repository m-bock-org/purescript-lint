module Test.Lint.PackagesSpec (spec) where

import Prelude

import Data.Array (concatMap, snoc) as Array
import Data.Array.NonEmpty (singleton) as NEA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains) as Str
import Lint (LintOptions, LintReport, lintPackages, lintWorkspace, runLinterIn) as Subject
import Lint.Internal.Exemptions (Standing(..)) as Exemptions
import Lint.Rule (perDecl) as Lint.Rule
import Lint.Rule.Survey
  ( Subject(..)
  , SurveyFinding
  , SurveyModule
  , WorkspaceLint
  , WorkspaceSurvey
  , perWorkspace_
  ) as Lint.Rule.Survey
import Lint.RuleSet (Rule, rule) as RuleSet
import Test.Lint.Fixture (inFixture)
import Test.Lint.ReadmeExample (maxFunctionArity)
import Test.Spec (Spec, around_, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = do
  describe "a scoped run" $ around_ inFixture do
    it "lints only the named package's modules, and counts only those" do
      (report :: Subject.LintReport) <-
        Subject.lintPackages (NEA.singleton "other") wholeRunOptions fixtureRules

      map _.moduleName report.located `shouldEqual` [ "Other" ]
      report.moduleCount `shouldEqual` 1

    it "keeps a workspace rule's findings on the named package, drops the rest" do
      (scoped :: Subject.LintReport) <-
        Subject.lintPackages (NEA.singleton "other") wholeRunOptions fixtureRules
      (whole :: Subject.LintReport) <-
        Subject.lintWorkspace wholeRunOptions fixtureRules

      scoped.total `shouldEqual` 2
      whole.total `shouldEqual` 5

    it "refuses a package the workspace does not have, naming it" do
      (verdict :: Either String Boolean) <-
        Subject.runLinterIn (NEA.singleton "otehr") wholeRunOptions fixtureRules

      case verdict of
        Left why -> Str.contains (Str.Pattern "no package called otehr") why `shouldEqual` true
        Right _ -> fail "a misspelled package was linted as a clean one"

wholeRunOptions :: Subject.LintOptions
wholeRunOptions =
  { skipModules: []
  , fix: Nothing
  , standing: Exemptions.All
  }

fixtureRules :: Array RuleSet.Rule
fixtureRules =
  [ RuleSet.rule (Lint.Rule.perDecl maxFunctionArity 0)
  , RuleSet.rule (Lint.Rule.Survey.perWorkspace_ everyModule)
  ]

everyModule :: Lint.Rule.Survey.WorkspaceLint Unit
everyModule =
  { name: "every-module"
  , description: "Flags every module, and the root namespace."
  , examples: Nothing
  , rule: const aboutEveryModule
  }

aboutEveryModule :: Lint.Rule.Survey.WorkspaceSurvey -> Array Lint.Rule.Survey.SurveyFinding
aboutEveryModule survey =
  let
    about :: Lint.Rule.Survey.SurveyModule -> Lint.Rule.Survey.SurveyFinding
    about one = { subject: Lint.Rule.Survey.Module one.moduleName, message: one.moduleName }
  in
    Array.snoc (Array.concatMap (map about <<< _.modules) survey.packages)
      { subject: Lint.Rule.Survey.Namespace "", message: "the root" }

-- ## Context
--
-- `spec`
--
-- The fixture has two packages, `fixture` holding `Fixture` and
-- `other` holding `Other`, and both modules have one binder for
-- `max-function-arity` at zero to report. Scoped to `other`: its
-- arity finding and `every-module`'s finding about `Other`. The whole
-- run adds `Fixture`'s two and the namespace finding nobody holds.
--
-- `everyModule`
--
-- A workspace rule with a finding about every module, and one about a
-- namespace, which no package holds - so a scoped run has both kinds
-- to drop.
