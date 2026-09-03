-- | The one string helper, and its edges.
module Test.Lint.Internal.DedentSpec (spec) where

import Prelude

import Lint.Internal.Dedent (dedent)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "dedent" do

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
