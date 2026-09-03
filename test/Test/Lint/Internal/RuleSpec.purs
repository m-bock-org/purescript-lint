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
  , disabled
  , exclude
  , fixed
  , perModule_
  , runRules
  , violations
  , withHint
  )
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Lint.Internal.Rule" do
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

alwaysViolates :: ModuleLint Unit
alwaysViolates =
  { name: "always-violates"
  , description: "Always fails, so a test can see what the runner does with a violation."
  , examples: Nothing
  , rule: \_config _context _mod -> violations [ "nope" ]
  }

alwaysFixes :: ModuleLint Unit
alwaysFixes =
  { name: "always-fixes"
  , description: "Always reports a fix, without actually changing anything."
  , examples: Nothing
  , rule: \_config _context mod -> fixed mod
  }

manyFindings :: ModuleLint Unit
manyFindings =
  { name: "many-findings"
  , description: "Reports three findings at once, the way a module-level rule does."
  , examples: Nothing
  , rule: \_config _context _mod -> violations [ "first", "second", "third" ]
  }

findsNothing :: ModuleLint Unit
findsNothing =
  { name: "finds-nothing"
  , description: "Computes an empty list of findings, which is how a rule passes."
  , examples: Nothing
  , rule: \_config _context _mod -> violations []
  }

hinted :: ModuleLint Unit
hinted =
  { name: "hinted"
  , description: "Carries a suggestion that belongs to the rule rather than to one finding."
  , examples: Nothing
  , rule: \_config _context _mod -> withHint "try harder" (violations [ "first", "second" ])
  }

runRulesSpec :: Spec Unit
runRulesSpec = describe "runRules" do

  it "names the rule that made each finding" do
    let
      outcome = runRules context [ { groups: [], rule: perModule_ alwaysViolates } ] sampleModule
    map (_.rule.name) outcome.violations `shouldEqual` [ "always-violates" ]

  it "collects a rule's violation" do
    let
      outcome = runRules context
        [ { groups: [], rule: perModule_ alwaysViolates } ]
        sampleModule
    map _.message outcome.violations `shouldEqual` [ "nope" ]
    outcome.fixed `shouldEqual` false

  it "reports nothing when no rule fires" do
    let outcome = runRules context ([] :: Array (Grouped ModuleRule)) sampleModule
    Array.length outcome.violations `shouldEqual` 0

  it "skips a disabled rule" do
    let
      outcome = runRules context
        [ { groups: [], rule: disabled true (perModule_ alwaysViolates) } ]
        sampleModule
    map _.message outcome.violations `shouldEqual` []

  it "skips a rule whose exemption applies" do
    let
      outcome = runRules context
        [ { groups: [], rule: exclude [ { name: "by design", appliesTo: \_ _ -> true } ]
            (perModule_ alwaysViolates) } ]
        sampleModule
    map _.message outcome.violations `shouldEqual` []

  it "runs a rule whose exemption does not apply" do
    let
      outcome = runRules context
        [ { groups: [], rule: exclude [ { name: "by design", appliesTo: \_ _ -> false } ]
            (perModule_ alwaysViolates) } ]
        sampleModule
    map _.message outcome.violations `shouldEqual` [ "nope" ]

  it "marks the outcome fixed when a rule rewrites" do
    let
      outcome = runRules context
        [ { groups: [], rule: perModule_ alwaysFixes } ]
        sampleModule
    outcome.fixed `shouldEqual` true
    map _.message outcome.violations `shouldEqual` []

  it "reports every finding a rule made, not just the first" do
    let
      outcome = runRules context
        [ { groups: [], rule: perModule_ manyFindings } ]
        sampleModule
    map _.message outcome.violations `shouldEqual` [ "first", "second", "third" ]

  it "passes when a rule finds nothing" do
    let
      outcome = runRules context
        [ { groups: [], rule: perModule_ findsNothing } ]
        sampleModule
    map _.message outcome.violations `shouldEqual` []

  it "attaches a hint to every finding, not just the first" do
    let
      outcome = runRules context
        [ { groups: [], rule: perModule_ hinted } ]
        sampleModule
    map _.message outcome.violations `shouldEqual` [ "first", "second" ]
    map _.hint outcome.violations `shouldEqual`
      [ Just "try harder", Just "try harder" ]

  it "runs every rule, not just the first to fire" do
    let
      outcome = runRules context
        [ { groups: [], rule: perModule_ alwaysViolates }, { groups: [], rule: perModule_ alwaysViolates } ]
        sampleModule
    map _.message outcome.violations `shouldEqual` [ "nope", "nope" ]

