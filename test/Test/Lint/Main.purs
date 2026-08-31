module Test.Lint.Main (main) where

import Prelude

import Effect (Effect)
import Test.Lint.DedentSpec as DedentSpec
import Test.Lint.RuleSetSpec as RuleSetSpec
import Test.Lint.RunRulesSpec as RunRulesSpec
import Test.Lint.WorkspaceSpec as WorkspaceSpec
import Test.Spec (describe)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main =
  runSpecAndExitProcess [ consoleReporter ] do
    describe "PureScript.Lint" do
      DedentSpec.spec
      RunRulesSpec.spec
      RuleSetSpec.spec
      WorkspaceSpec.spec
