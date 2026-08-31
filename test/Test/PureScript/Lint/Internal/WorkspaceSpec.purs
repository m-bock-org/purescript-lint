module Test.PureScript.Lint.Internal.WorkspaceSpec (spec) where

import Prelude

import Data.String (contains) as Str
import Data.String.Pattern (Pattern(..))
import PureScript.Lint.Internal.Workspace (moduleGlob)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "moduleGlob" do

  it "builds a glob for a package nested in the repo" do
    moduleGlob "packages/parser" "src" `shouldEqual` "packages/parser/src/**/*.purs"

  it "builds a glob for a package at the repo root, which spago reports as ./" do
    moduleGlob "./" "src" `shouldEqual` "src/**/*.purs"

  it "never produces a doubled separator, which matches nothing" do
    Str.contains (Pattern "//") (moduleGlob "./" "src") `shouldEqual` false
    Str.contains (Pattern "//") (moduleGlob "." "test") `shouldEqual` false
    Str.contains (Pattern "//") (moduleGlob "pkg/" "src") `shouldEqual` false

  it "covers the test tree the same way" do
    moduleGlob "./" "test" `shouldEqual` "test/**/*.purs"
