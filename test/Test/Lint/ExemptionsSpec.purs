-- | The exemption file, read end to end.
-- |
-- | Not a unit test of the matcher: the question worth answering is
-- | whether a file sitting in a repository actually silences a rule,
-- | and only a real run answers that.
module Test.Lint.ExemptionsSpec (spec) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (filter, length) as Array
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff (attempt) as Aff
import Lint (lintWorkspace)
import Lint.Internal.Exemptions (decodeExemptions, exemptFile, matches)
import Lint.Rule (perDecl)
import Lint.RuleSet (Rule)
import Lint.RuleSet as RuleSet
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Test.Lint.ReadmeExample (maxFunctionArity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "the exemption file" do
    it "silences a rule where it says to" do
      without <- withoutExemptions countFindings
      with <- withExemptions silencing countFindings
      -- Fewer, not zero: the exemption names one rule in one
      -- namespace, so everything outside it must still be found.
      (with < without) `shouldEqual` true
      (with > 0) `shouldEqual` true

    it "and an absent file exempts nothing" do
      with <- withExemptions silencing countFindings
      without <- withoutExemptions countFindings
      -- The same exemption, and then no file at all. Taking the file
      -- away has to bring the findings back, or this suite would pass
      -- against a linter that never read it. The old version of this
      -- compared one count to itself and asserted nothing.
      (without > with) `shouldEqual` true

  describe "matching" do
    it "takes a trailing star as a prefix" do
      matches sample { rule: "r", moduleName: "A.B.C", path: "", declarationName: Nothing }
        `shouldEqual` true
    it "and does not match a different prefix" do
      matches sample { rule: "r", moduleName: "X.Y", path: "", declarationName: Nothing }
        `shouldEqual` false
    it "matches a path by its end, so a package need not be named" do
      matches byPath
        { rule: "r", moduleName: "Any", path: "purs/deep/Scratch.purs", declarationName: Nothing }
        `shouldEqual` true
    it "a rule of * covers every rule" do
      matches sample { rule: "anything", moduleName: "A.B", path: "", declarationName: Nothing }
        `shouldEqual` true

-- | Private. Used by the specs.
sample :: Array { modules :: Array String, paths :: Array String, rule :: String, why :: String }
sample = [ { rule: "*", modules: [ "A.*" ], paths: [], why: "because" } ]

-- | Private. Used by the specs.
byPath :: Array { modules :: Array String, paths :: Array String, rule :: String, why :: String }
byPath = [ { rule: "*", modules: [], paths: [ "Scratch.purs" ], why: "because" } ]

-- | Private. Used by the specs. Uses `rules`.
countFindings :: Aff Int
countFindings = do
  report <- lintWorkspace { skipModules: [], fix: Nothing } rules
  pure (Array.length (Array.filter inInternal report.located))

-- | Private. Used only by `countFindings`.
inInternal :: forall r. { moduleName :: String | r } -> Boolean
inInternal _ = true

-- | Private. Used by the specs.
silencing :: String
silencing =
  """
  { "exempt":
    [ { "rule": "max-function-arity"
      , "modules": ["Lint.Internal.*"]
      , "why": "proving the file is read"
      }
    ]
  }
  """

-- | Private. Used by the specs. Uses `restoring`.
withExemptions :: forall a. String -> Aff a -> Aff a
withExemptions text action = restoring do
  FS.writeTextFile UTF8 exemptFile text
  action

-- | Private. Used by the specs. Uses `restoring`.
withoutExemptions :: forall a. Aff a -> Aff a
withoutExemptions action = restoring do
  _ <- Aff.attempt (FS.unlink exemptFile)
  action

-- | Private, depth 2. Used by `withExemptions`, `withoutExemptions`.
-- |
-- | The saving is the point. `exemptFile` is a fixed name in the
-- | current directory, and the current directory when the suite runs
-- | is this repository - so a spec that writes it writes the
-- | linter's own exemption list. An earlier version wrote over it and
-- | then unlinked it, which left the working tree missing a tracked
-- | file after every green test run, and the next lint applying no
-- | exemptions at all. Nothing failed; the file was simply gone.
-- |
-- | Restores through a failure too, or the first broken assertion
-- | leaves the repository in that state again.
restoring :: forall a. Aff a -> Aff a
restoring action = do
  saved <- Aff.attempt (FS.readTextFile UTF8 exemptFile)
  outcome <- Aff.attempt action
  case saved of
    Left _ -> void (Aff.attempt (FS.unlink exemptFile))
    Right text -> FS.writeTextFile UTF8 exemptFile text
  either throwError pure outcome

-- | Private. Used by `countFindings`.
rules :: Array Rule
rules = [ RuleSet.rule (perDecl maxFunctionArity 0) ]
