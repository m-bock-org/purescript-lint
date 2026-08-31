module Test.Lint.ReadmeExampleSpec (spec) where

import Prelude

import Data.Array (concatMap) as Array
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types (Declaration, Module(..), ModuleBody(..)) as CST
import Lint.Internal.Rule (DeclarationRule, LintContext, ModuleKind(..), perDecl, runRules)
import Test.Lint.ReadmeExample (arityRule, arityUnlessGenerated, maxFunctionArity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

declarationsOf :: String -> Array (CST.Declaration Void)
declarationsOf src = case parseModule src of
  ParseSucceeded (CST.Module { body: CST.ModuleBody { decls } }) -> decls
  _ -> unsafeCrashWith "ReadmeExampleSpec fixture: source does not parse"

context :: LintContext
context =
  { packageName: "sample-pkg"
  , moduleName: "Sample"
  , declarationName: Nothing
  , path: "src/Sample.purs"
  , kind: SourceModule
  }

messagesFor :: Int -> String -> Array String
messagesFor maxArity src =
  Array.concatMap
    (\decl -> map _.message (runRules context [ { groups: [], rule: perDecl maxFunctionArity maxArity } ] decl).violations)
    (declarationsOf src)

inModule :: String -> DeclarationRule -> String -> Array String
inModule moduleName aRule src =
  Array.concatMap
    (\decl -> map _.message (runRules (context { moduleName = moduleName }) [ { groups: [], rule: aRule } ] decl).violations)
    (declarationsOf src)

spec :: Spec Unit
spec = describe "the rule the README shows" do

  it "flags a function with more arguments than allowed" do
    messagesFor 2 "module S where\n\nresize w h q img = img\n"
      `shouldEqual` [ "resize takes 4 args" ]

  it "passes a function at the limit" do
    messagesFor 2 "module S where\n\nresize opts img = img\n"
      `shouldEqual` []

  it "says nothing about a declaration that takes no arguments" do
    messagesFor 2 "module S where\n\nvalue = 1\n"
      `shouldEqual` []

  it "fires in an ordinary module, exemption or not" do
    let src = "module S where\n\nresize a b c d e = e\n"
    inModule "MyApp.Image" arityRule src `shouldEqual` [ "resize takes 5 args" ]
    inModule "MyApp.Image" arityUnlessGenerated src `shouldEqual` [ "resize takes 5 args" ]

  it "is suppressed by the exemption in a generated module" do
    let src = "module S where\n\nresize a b c d e = e\n"
    inModule "MyApp.Generated.Image" arityRule src `shouldEqual` [ "resize takes 5 args" ]
    inModule "MyApp.Generated.Image" arityUnlessGenerated src `shouldEqual` []
