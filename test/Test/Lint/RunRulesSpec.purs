module Test.Lint.RunRulesSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types (Module) as CST
import PureScript.Lint.Rule
  ( LintContext
  , ModuleLint
  , ModuleRule
  , RuleName(..)
  , disabled
  , exclude
  , fixed
  , perModule
  , runRules
  , violation
  , violations
  , withHint
  )
import PureScript.Lint.Workspace (ModuleKind(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | Private. Used only by `spec`.
sampleModule :: CST.Module Void
sampleModule = case parseModule "module Sample where\n\nvalue :: Int\nvalue = 1\n" of
  ParseSucceeded m -> m
  _ -> unsafeCrashWith "RunRulesSpec fixture: sample module does not parse"

-- | Private. Used only by `spec`.
context :: LintContext
context =
  { packageName: "sample-pkg"
  , moduleName: "Sample"
  , declarationName: Nothing
  , path: "src/Sample.purs"
  , kind: SourceModule
  }

-- | Private. Used only by `spec`.
alwaysViolates :: ModuleLint
alwaysViolates =
  { name: RuleName "always-violates"
  , description: "Always fails, so a test can see what the runner does with a violation."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context _mod -> violation "nope"
  }

-- | Private. Used only by `spec`.
alwaysFixes :: ModuleLint
alwaysFixes =
  { name: RuleName "always-fixes"
  , description: "Always reports a fix, without actually changing anything."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context mod -> fixed mod
  }

-- | Private. Used only by `spec`.
manyFindings :: ModuleLint
manyFindings =
  { name: RuleName "many-findings"
  , description: "Reports three findings at once, the way a module-level rule does."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context _mod -> violations [ "first", "second", "third" ]
  }

-- | Private. Used only by `spec`.
findsNothing :: ModuleLint
findsNothing =
  { name: RuleName "finds-nothing"
  , description: "Computes an empty list of findings, which is how a rule passes."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context _mod -> violations []
  }

-- | Private. Used only by `spec`.
hinted :: ModuleLint
hinted =
  { name: RuleName "hinted"
  , description: "Carries a suggestion that belongs to the rule rather than to one finding."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context _mod -> withHint "try harder" (violations [ "first", "second" ])
  }

spec :: Spec Unit
spec = describe "runRules" do

  it "collects a rule's violation" do
    let
      outcome = runRules context
        [ perModule alwaysViolates ]
        sampleModule
    outcome.violations `shouldEqual` [ "nope" ]
    outcome.fixed `shouldEqual` false

  it "reports nothing when no rule fires" do
    let outcome = runRules context ([] :: Array ModuleRule) sampleModule
    Array.length outcome.violations `shouldEqual` 0

  it "skips a disabled rule" do
    let
      outcome = runRules context
        [ disabled true (perModule alwaysViolates) ]
        sampleModule
    outcome.violations `shouldEqual` []

  it "skips a rule whose exemption applies" do
    let
      outcome = runRules context
        [ exclude [ { name: "by design", appliesTo: \_ _ -> true } ]
            (perModule alwaysViolates)
        ]
        sampleModule
    outcome.violations `shouldEqual` []

  it "runs a rule whose exemption does not apply" do
    let
      outcome = runRules context
        [ exclude [ { name: "by design", appliesTo: \_ _ -> false } ]
            (perModule alwaysViolates)
        ]
        sampleModule
    outcome.violations `shouldEqual` [ "nope" ]

  it "marks the outcome fixed when a rule rewrites" do
    let
      outcome = runRules context
        [ perModule alwaysFixes ]
        sampleModule
    outcome.fixed `shouldEqual` true
    outcome.violations `shouldEqual` []

  it "reports every finding a rule made, not just the first" do
    let
      outcome = runRules context
        [ perModule manyFindings ]
        sampleModule
    outcome.violations `shouldEqual` [ "first", "second", "third" ]

  it "passes when a rule finds nothing" do
    let
      outcome = runRules context
        [ perModule findsNothing ]
        sampleModule
    outcome.violations `shouldEqual` []

  it "attaches a hint to every finding, not just the first" do
    let
      outcome = runRules context
        [ perModule hinted ]
        sampleModule
    outcome.violations `shouldEqual`
      [ "first (hint: try harder)", "second (hint: try harder)" ]

  it "runs every rule, not just the first to fire" do
    let
      outcome = runRules context
        [ perModule alwaysViolates, perModule alwaysViolates ]
        sampleModule
    outcome.violations `shouldEqual` [ "nope", "nope" ]
