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
import Lint.Internal.Exemptions as Exemptions
import Lint.Internal.Exemptions (exemptFile, matches)
import Lint.Rule (perDecl)
import Lint.RuleSet (Rule)
import Lint.RuleSet as RuleSet
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Test.Lint.ReadmeExample (maxFunctionArity)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- | Uses `withoutExemptions`, `withExemptions`.
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

    it "suppresses a pending entry for an ordinary run" do
      without <- withoutExemptions countFindings
      with <- withExemptions pendingOnly countFindings
      -- Pending is still an exemption to everyone but the fixer: a
      -- backlog somebody else wrote must not fail the build of the
      -- person editing an unrelated module.
      (with < without) `shouldEqual` true

    it "but shows it to a run that honours by-design only" do
      suppressed <- withExemptions pendingOnly countFindings
      exposed <- withExemptions pendingOnly countFindingsForFixer
      -- The same file, read two ways. This is what lets the nightly
      -- fixer see the backlog it is meant to work through while every
      -- other run stays green.
      (exposed > suppressed) `shouldEqual` true

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

-- | Private.
sample :: Array { modules :: Array String, paths :: Array String, rule :: String, why :: String }
sample = [ { rule: "*", modules: [ "A.*" ], paths: [], why: "because" } ]

-- | Private.
byPath :: Array { modules :: Array String, paths :: Array String, rule :: String, why :: String }
byPath = [ { rule: "*", modules: [], paths: [ "Scratch.purs" ], why: "because" } ]

-- | Private. Uses `inInternal`.
countFindings :: Aff Int
countFindings = do
  report <- lintWorkspace { skipModules: [], fix: Nothing, standing: Exemptions.All } rules
  pure (Array.length (Array.filter inInternal report.located))

-- | Private.
inInternal :: ∀ r. { moduleName :: String | r } -> Boolean
inInternal _ = true

-- | Private.
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

-- | Private. Used only by `spec`. Uses `restoring`.
withExemptions :: ∀ a. String -> Aff a -> Aff a
withExemptions text action = restoring do
  FS.writeTextFile UTF8 exemptFile text
  action

-- | as debt rather than as a decision.
-- | Private.
pendingOnly :: String
pendingOnly =
  """
  { "pending":
    [ { "rule": "max-function-arity"
      , "modules": ["Lint.Internal.*"]
      , "why": "proving the pending list is read"
      }
    ]
  }
  """

-- | Private. Uses `inInternal`.
countFindingsForFixer :: Aff Int
countFindingsForFixer = do
  report <- lintWorkspace
    { skipModules: [], fix: Nothing, standing: Exemptions.ByDesignOnly }
    rules
  pure (Array.length (Array.filter inInternal report.located))

-- | Private. Used only by `spec`. Uses `restoring`.
withoutExemptions :: ∀ a. Aff a -> Aff a
withoutExemptions action = restoring do
  _ <- Aff.attempt (FS.unlink exemptFile)
  action

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
-- | Private.
restoring :: ∀ a. Aff a -> Aff a
restoring action = do
  saved <- Aff.attempt (FS.readTextFile UTF8 exemptFile)
  outcome <- Aff.attempt action
  case saved of
    Left _ -> void (Aff.attempt (FS.unlink exemptFile))
    Right text -> FS.writeTextFile UTF8 exemptFile text
  either throwError pure outcome

-- | Private.
rules :: Array Rule
rules = [ RuleSet.rule (perDecl maxFunctionArity 0) ]
