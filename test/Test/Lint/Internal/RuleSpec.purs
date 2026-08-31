module Test.Lint.Internal.RuleSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types (Module) as CST
import Lint.Internal.Rule
  ( LintContext
  , ModuleKind(..)
  , ModuleLint
  , Grouped
  , ModuleRule
  , dedent
  , disabled
  , exclude
  , fixed
  , perModule
  , runRules
  , violations
  , withHint
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Lint.Internal.Rule" do
  dedentSpec
  runRulesSpec

sampleModule :: CST.Module Void
sampleModule = case parseModule "module Sample where\n\nvalue :: Int\nvalue = 1\n" of
  ParseSucceeded m -> m
  _ -> unsafeCrashWith "RunRulesSpec fixture: sample module does not parse"

context :: LintContext
context =
  { packageName: "sample-pkg"
  , moduleName: "Sample"
  , declarationName: Nothing
  , path: "src/Sample.purs"
  , kind: SourceModule
  }

alwaysViolates :: ModuleLint
alwaysViolates =
  { name: "always-violates"
  , description: "Always fails, so a test can see what the runner does with a violation."
  , goodExamples: []
  , badExamples: []
  , exampleConfig: Nothing
  , rule: \_context _mod -> violations [ "nope" ]
  }

alwaysFixes :: ModuleLint
alwaysFixes =
  { name: "always-fixes"
  , description: "Always reports a fix, without actually changing anything."
  , goodExamples: []
  , badExamples: []
  , exampleConfig: Nothing
  , rule: \_context mod -> fixed mod
  }

manyFindings :: ModuleLint
manyFindings =
  { name: "many-findings"
  , description: "Reports three findings at once, the way a module-level rule does."
  , goodExamples: []
  , badExamples: []
  , exampleConfig: Nothing
  , rule: \_context _mod -> violations [ "first", "second", "third" ]
  }

findsNothing :: ModuleLint
findsNothing =
  { name: "finds-nothing"
  , description: "Computes an empty list of findings, which is how a rule passes."
  , goodExamples: []
  , badExamples: []
  , exampleConfig: Nothing
  , rule: \_context _mod -> violations []
  }

hinted :: ModuleLint
hinted =
  { name: "hinted"
  , description: "Carries a suggestion that belongs to the rule rather than to one finding."
  , goodExamples: []
  , badExamples: []
  , exampleConfig: Nothing
  , rule: \_context _mod -> withHint "try harder" (violations [ "first", "second" ])
  }

runRulesSpec :: Spec Unit
runRulesSpec = describe "runRules" do

  it "names the rule that made each finding" do
    let
      outcome = runRules context [ { groups: [], rule: perModule alwaysViolates } ] sampleModule
    map (_.rule.name) outcome.violations `shouldEqual` [ "always-violates" ]

  it "collects a rule's violation" do
    let
      outcome = runRules context
        [ { groups: [], rule: perModule alwaysViolates } ]
        sampleModule
    map _.message outcome.violations `shouldEqual` [ "nope" ]
    outcome.fixed `shouldEqual` false

  it "reports nothing when no rule fires" do
    let outcome = runRules context ([] :: Array (Grouped ModuleRule)) sampleModule
    Array.length outcome.violations `shouldEqual` 0

  it "skips a disabled rule" do
    let
      outcome = runRules context
        [ { groups: [], rule: disabled true (perModule alwaysViolates) } ]
        sampleModule
    map _.message outcome.violations `shouldEqual` []

  it "skips a rule whose exemption applies" do
    let
      outcome = runRules context
        [ { groups: [], rule: exclude [ { name: "by design", appliesTo: \_ _ -> true } ]
            (perModule alwaysViolates) } ]
        sampleModule
    map _.message outcome.violations `shouldEqual` []

  it "runs a rule whose exemption does not apply" do
    let
      outcome = runRules context
        [ { groups: [], rule: exclude [ { name: "by design", appliesTo: \_ _ -> false } ]
            (perModule alwaysViolates) } ]
        sampleModule
    map _.message outcome.violations `shouldEqual` [ "nope" ]

  it "marks the outcome fixed when a rule rewrites" do
    let
      outcome = runRules context
        [ { groups: [], rule: perModule alwaysFixes } ]
        sampleModule
    outcome.fixed `shouldEqual` true
    map _.message outcome.violations `shouldEqual` []

  it "reports every finding a rule made, not just the first" do
    let
      outcome = runRules context
        [ { groups: [], rule: perModule manyFindings } ]
        sampleModule
    map _.message outcome.violations `shouldEqual` [ "first", "second", "third" ]

  it "passes when a rule finds nothing" do
    let
      outcome = runRules context
        [ { groups: [], rule: perModule findsNothing } ]
        sampleModule
    map _.message outcome.violations `shouldEqual` []

  it "attaches a hint to every finding, not just the first" do
    let
      outcome = runRules context
        [ { groups: [], rule: perModule hinted } ]
        sampleModule
    map _.message outcome.violations `shouldEqual` [ "first", "second" ]
    map _.hint outcome.violations `shouldEqual`
      [ Just "try harder", Just "try harder" ]

  it "runs every rule, not just the first to fire" do
    let
      outcome = runRules context
        [ { groups: [], rule: perModule alwaysViolates }, { groups: [], rule: perModule alwaysViolates } ]
        sampleModule
    map _.message outcome.violations `shouldEqual` [ "nope", "nope" ]

dedentSpec :: Spec Unit
dedentSpec = describe "dedent" do

  it "strips the common indentation from every line" do
    dedent "    foo\n    bar" `shouldEqual` "foo\nbar"

  it "keeps relative indentation" do
    dedent "    foo\n      bar" `shouldEqual` "foo\n  bar"

  it "drops a leading blank line, as a triple-quoted string leaves behind" do
    dedent "\n    foo\n    bar" `shouldEqual` "foo\nbar"

  it "drops a trailing blank line too" do
    dedent "\n    foo\n    bar\n" `shouldEqual` "foo\nbar"

  it "leaves an already-flush string alone" do
    dedent "foo\nbar" `shouldEqual` "foo\nbar"

  it "measures the common prefix across all lines, not just the first" do
    dedent "      foo\n    bar" `shouldEqual` "  foo\nbar"

  it "handles a single line" do
    dedent "    foo" `shouldEqual` "foo"

  it "handles the empty string" do
    dedent "" `shouldEqual` ""
