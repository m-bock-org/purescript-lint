module Test.Lint.Internal.RuleSetSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Maybe (Maybe(..))
import Lint.Rule
  ( DeclarationLint
  , ExprLint
  , ModuleLint
  , perDecl_
  , perExpr_
  , perModule_
  , violations
  )
import Lint.Internal.RuleSet (flattenRules, group, rule)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | Private.
noopModule :: ModuleLint Unit
noopModule =
  { name: "noop-module"
  , description: "Passes. Only its shape matters here."
  , examples: Nothing
  , rule: \_config _context _value -> violations []
  }

-- | Private.
noopDecl :: DeclarationLint Unit
noopDecl =
  { name: "noop-decl"
  , description: "Passes. Only its shape matters here."
  , examples: Nothing
  , rule: \_config _context _value -> violations []
  }

-- | Private.
noopExpr :: ExprLint Unit
noopExpr =
  { name: "noop-expr"
  , description: "Passes. Only its shape matters here."
  , examples: Nothing
  , rule: \_config _context _value -> violations []
  }

spec :: Spec Unit
spec = describe "flattenRules" do

  it "sorts each rule into the bucket for the syntax it sees" do
    let
      flat = flattenRules
        [ rule (perModule_ noopModule)
        , rule (perDecl_ noopDecl)
        , rule (perExpr_ noopExpr)
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
    let flat = flattenRules [ group "a heading" [ rule (perModule_ noopModule) ] ]
    Array.length flat.modules `shouldEqual` 1

  it "sees through nested groups" do
    let
      flat = flattenRules
        [ group "outer" [ group "inner" [ rule (perDecl_ noopDecl) ] ] ]
    Array.length flat.declarations `shouldEqual` 1

  it "keeps a rule listed twice, twice" do
    let
      flat = flattenRules [ rule (perModule_ noopModule), rule (perModule_ noopModule) ]
    Array.length flat.modules `shouldEqual` 2

  it "concatenates two rule sets, which is how one builds on another" do
    let
      quick = [ rule (perModule_ noopModule) ]
      rest = [ rule (perDecl_ noopDecl) ]
      flat = flattenRules (quick <> rest)
    Array.length flat.modules `shouldEqual` 1
    Array.length flat.declarations `shouldEqual` 1
