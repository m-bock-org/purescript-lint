module Test.Main (main) where

import Prelude

import Effect (Effect)
import Test.PureScript.Lint.Internal.RuleSetSpec as RuleSetSpec
import Test.PureScript.Lint.Internal.RuleSpec as RuleSpec
import Test.PureScript.Lint.Internal.WorkspaceSpec as WorkspaceSpec
import Test.PureScript.Lint.ReadmeExampleSpec as ReadmeExampleSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main =
  runSpecAndExitProcess [ consoleReporter ] do
    RuleSpec.spec
    RuleSetSpec.spec
    WorkspaceSpec.spec
    ReadmeExampleSpec.spec
