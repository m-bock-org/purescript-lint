module Test.Lint.RuleSetSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Maybe (Maybe(..))
import PureScript.Lint.Rule
  ( DeclarationLint
  , ExprLint
  , LintResult(..)
  , ModuleLint
  , RuleName(..)
  , perDecl
  , perExpr
  , perModule
  )
import PureScript.Lint.RuleSet (flattenRules, group, rule)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | Private. Used only by `spec`.
noopModule :: ModuleLint
noopModule =
  { name: RuleName "noop-module"
  , description: "Passes. Only its shape matters here."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context _value -> Passed
  }

-- | Private. Used only by `spec`.
noopDecl :: DeclarationLint
noopDecl =
  { name: RuleName "noop-decl"
  , description: "Passes. Only its shape matters here."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context _value -> Passed
  }

-- | Private. Used only by `spec`.
noopExpr :: ExprLint
noopExpr =
  { name: RuleName "noop-expr"
  , description: "Passes. Only its shape matters here."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context _value -> Passed
  }

spec :: Spec Unit
spec = describe "flattenRules" do

  it "sorts each rule into the bucket for the syntax it sees" do
    let
      flat = flattenRules
        [ rule (perModule noopModule)
        , rule (perDecl noopDecl)
        , rule (perExpr noopExpr)
        ]
    Array.length flat.modules `shouldEqual` 1
    Array.length flat.declarations `shouldEqual` 1
    Array.length flat.expressions `shouldEqual` 1

  it "is empty for no rules" do
    let flat = flattenRules []
    Array.length flat.modules `shouldEqual` 0
    Array.length flat.declarations `shouldEqual` 0
    Array.length flat.expressions `shouldEqual` 0
    Array.length flat.packages `shouldEqual` 0
    Array.length flat.workspaces `shouldEqual` 0

  it "sees through a group, which is presentation only" do
    let flat = flattenRules [ group "a heading" [ rule (perModule noopModule) ] ]
    Array.length flat.modules `shouldEqual` 1

  it "sees through nested groups" do
    let
      flat = flattenRules
        [ group "outer" [ group "inner" [ rule (perDecl noopDecl) ] ] ]
    Array.length flat.declarations `shouldEqual` 1

  it "keeps a rule listed twice, twice" do
    let
      flat = flattenRules [ rule (perModule noopModule), rule (perModule noopModule) ]
    Array.length flat.modules `shouldEqual` 2

  it "concatenates two rule sets, which is how one builds on another" do
    let
      quick = [ rule (perModule noopModule) ]
      rest = [ rule (perDecl noopDecl) ]
      flat = flattenRules (quick <> rest)
    Array.length flat.modules `shouldEqual` 1
    Array.length flat.declarations `shouldEqual` 1
