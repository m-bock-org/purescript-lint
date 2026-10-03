-- | The exemption files, read end to end over the fixture workspace.
module Test.Lint.ExemptionsSpec (spec) where

import Prelude

import Data.Array (filter, length) as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains) as Str
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff (attempt) as Aff
import Lint (LintReport, Located, lintWorkspace, oldestFirst, recordedIn)
import Lint.Internal.Exemptions (Exempt, ExemptionQuery, Exemptions, Kind(..), exemptFile, matches)
import Lint.Internal.Exemptions (Standing(..), decodeExemptions, decodeExemptionsFrom, readExemptionsWith) as Exemptions
import Lint.Rule (RuleId(..), perDecl)
import Lint.RuleSet (Rule)
import Lint.RuleSet (rule) as RuleSet
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile) as FS
import Test.Lint.Fixture (inFixture, packageFile)
import Test.Lint.ReadmeExample (maxFunctionArity)
import Test.Spec (Spec, around_, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- | Uses `countFindings`, `countFindingsForFixer`, `outcomeOf`, `findingIn`.
spec :: Spec Unit
spec = do
  describe "the exemption files" $ around_ inFixture do
    it "silences a rule where the root file says to" do
      (without :: Int) <- countFindings
      FS.writeTextFile UTF8 exemptFile silencing
      (with :: Int) <- countFindings

      (without /\ with) `shouldEqual` (2 /\ 1)

    it "a package's file with modules: * covers that package and no other" do
      FS.writeTextFile UTF8 (packageFile "pkg") everythingHere
      (left :: Array String) <- modulesWithFindings

      left `shouldEqual` [ "Other" ]

    it "and the root file's * covers every package" do
      FS.writeTextFile UTF8 exemptFile everythingHere
      (with :: Int) <- countFindings

      with `shouldEqual` 0

    it "suppresses a backlog entry for an ordinary run" do
      FS.writeTextFile UTF8 exemptFile pendingOnly
      (with :: Int) <- countFindings

      with `shouldEqual` 1

    it "but shows it to a run that honours by-design only" do
      FS.writeTextFile UTF8 exemptFile pendingOnly
      (exposed :: Int) <- countFindingsForFixer

      exposed `shouldEqual` 2

    it "names the file a malformed entry is in" do
      FS.writeTextFile UTF8 (packageFile "other")
        "exemptions:\n  - rule: r\n    kind: sometimes\n"
      (failed :: Either String Int) <- outcomeOf countFindings

      case failed of
        Left why -> Str.contains (Str.Pattern (packageFile "other")) why `shouldEqual` true
        Right _ -> fail "a kind nobody defined was read as something"

    it "refuses the old file's `exempt` key rather than reading nothing" do
      FS.writeTextFile UTF8 (packageFile "pkg")
        "exempt:\n  - rule: max-function-arity\n    modules: [\"*\"]\n"
      (failed :: Either String Int) <- outcomeOf countFindings

      case failed of
        Left why -> Str.contains (Str.Pattern "`exemptions`") why `shouldEqual` true
        Right _ -> fail "the old key was read as a file with nothing in it"

    it "refuses a package's file under the old name, naming it" do
      FS.writeTextFile UTF8 "pkg/lint-exemptions.json" "{\"exemptions\": []}"
      (failed :: Either String Int) <- outcomeOf countFindings

      case failed of
        Left why -> Str.contains (Str.Pattern "pkg/lint-exemptions.json: rename to lint-exemptions.yaml") why
          `shouldEqual` true
        Right _ -> fail "a file under the old name was read, or skipped, rather than refused"

    it "and the root's, even beside one under the new name" do
      FS.writeTextFile UTF8 exemptFile silencing
      FS.writeTextFile UTF8 "lint-exemptions.json" "{\"exemptions\": []}"
      (failed :: Either String Int) <- outcomeOf countFindings

      case failed of
        Left why -> Str.contains (Str.Pattern "lint-exemptions.json: rename to lint-exemptions.yaml") why
          `shouldEqual` true
        Right _ -> fail "the root's old file was skipped rather than refused"

    it "stamps an entry with the path it was read from, and the fixer's line names it" do
      FS.writeTextFile UTF8 (packageFile "pkg") dated
      (stamped :: Either String Exemptions) <- Exemptions.readExemptionsWith Exemptions.All fixturePackages
      (seen :: Array Located) <- findingsForFixer

      case stamped, Array.filter (\(one :: Located) -> one.moduleName == "Fixture") seen of
        Right [ one ], [ finding ] -> do
          one.file `shouldEqual` packageFile "pkg"
          one.package `shouldEqual` Just "fixture"
          recordedIn [ one ] finding `shouldEqual` " (backlog in pkg/lint-exemptions.yaml since 2026-09-13)"
        Right entries, findings ->
          fail ("expected one entry and one finding, got " <> show (Array.length entries) <> " and " <> show (Array.length findings))
        Left why, _ -> fail why

  describe "since" do
    it "reads the day it was written" do
      case Exemptions.decodeExemptions dated of
        Left why -> fail why
        Right [ one ] -> one.since `shouldEqual` "2026-09-13"
        Right _ -> fail "expected exactly one exemption"

    it "is empty when nobody said, rather than invented" do
      case Exemptions.decodeExemptions silencing of
        Left why -> fail why
        Right [ one ] -> one.since `shouldEqual` ""
        Right _ -> fail "expected exactly one exemption"

  describe "where an entry came from" do
    it "is the file it was read from, and the package that file sits in" do
      let
        origin :: { file :: String, package :: Maybe String }
        origin =
          { file: "pkg/lint-exemptions.yaml"
          , package: Just "fixture"
          }

      case Exemptions.decodeExemptionsFrom Exemptions.All origin silencing of
        Left why -> fail why
        Right [ one ] -> (one.file /\ one.package) `shouldEqual` (origin.file /\ origin.package)
        Right _ -> fail "expected exactly one exemption"

  describe "matching" do
    it "takes a trailing star as a prefix" do
      matches prefixed (ruleOn "r" "A.B.C") `shouldEqual` true
    it "and does not match a different prefix" do
      matches prefixed (ruleOn "r" "X.Y") `shouldEqual` false
    it "matches a path by its end, so a package need not be named" do
      matches byPath (ruleOn "r" "Any") { path = "purs/deep/Scratch.purs" } `shouldEqual` true
    it "a rule of * covers every rule" do
      matches prefixed (ruleOn "anything" "A.B") `shouldEqual` true
    it "a package's entry answers for its package" do
      matches scoped (ruleOn "r" "A.B") `shouldEqual` true
    it "and not for another" do
      matches scoped (ruleOn "r" "A.B") { packageName = "elsewhere" } `shouldEqual` false

  describe "oldestFirst" do
    it "tries what no entry covers, then the oldest debt, then the undated" do
      let
        unranked :: Array Located
        unranked = [ findingIn "Undated", findingIn "Old", findingIn "Fresh", findingIn "New" ]

        ranked :: Array String
        ranked = map _.moduleName (oldestFirst recorded unranked)

      ranked `shouldEqual` [ "Fresh", "Old", "New", "Undated" ]

    it "ranks across the root's file and a package's alike" do
      let
        acrossFiles :: Exemptions
        acrossFiles =
          [ (exemptionOf [ "Old" ] Backlog "2026-09-01") { file = packageFile "pkg", package = Just "here" }
          , exemptionOf [ "New" ] Backlog "2026-09-20"
          ]

        ranked :: Array String
        ranked = map _.moduleName (oldestFirst acrossFiles [ findingIn "New", findingIn "Old" ])

      ranked `shouldEqual` [ "Old", "New" ]

    it "covers a finding about one declaration with a Module#name entry" do
      let
        onDeclaration :: Exemptions
        onDeclaration = [ exemptionOf [ "M#covered" ] Backlog "2026-09-01" ]

        covered :: Located
        covered = (findingIn "M") { finding { declarationName = Just "covered" } }

        uncovered :: Located
        uncovered = (findingIn "M") { finding { declarationName = Just "uncovered" } }

        ranked :: Array (Maybe String)
        ranked = map _.finding.declarationName (oldestFirst onDeclaration [ covered, uncovered ])

      ranked `shouldEqual` [ Just "uncovered", Just "covered" ]
      recordedIn onDeclaration covered `shouldEqual` " (backlog in lint-exemptions.yaml since 2026-09-01)"

-- | Private. Used only by `spec`.
countFindings :: Aff Int
countFindings = do
  (report :: LintReport) <- lintWorkspace
    { skipModules: []
    , fix: Nothing
    , standing: Exemptions.All
    }
    rules
  pure (Array.length report.located)

-- | Private. Used only by `spec`.
modulesWithFindings :: Aff (Array String)
modulesWithFindings = do
  (report :: LintReport) <- lintWorkspace
    { skipModules: []
    , fix: Nothing
    , standing: Exemptions.All
    }
    rules
  pure (map _.moduleName report.located)

-- | Private. Used only by `spec`, `countFindingsForFixer`.
findingsForFixer :: Aff (Array Located)
findingsForFixer = do
  (report :: LintReport) <- lintWorkspace
    { skipModules: []
    , fix: Nothing
    , standing: Exemptions.ByDesignOnly
    }
    rules
  pure report.located

-- | Private. Used only by `spec`.
countFindingsForFixer :: Aff Int
countFindingsForFixer = do
  (seen :: Array Located) <- findingsForFixer
  pure (Array.length seen)

-- | The fixture's packages, as `spago ls packages` would name them.
-- | Private.
fixturePackages :: Array { name :: String, path :: String }
fixturePackages = [ { name: "fixture", path: "pkg" }, { name: "other", path: "other" } ]

-- | The run's answer, or the words it failed with - a malformed file
-- | throws out of `lintWorkspace`, and the words are what a spec checks.
-- | Private. Used only by `spec`.
outcomeOf :: ∀ a. Aff a -> Aff (Either String a)
outcomeOf action = do
  (attempted :: Either _ a) <- Aff.attempt action
  pure (lmap show attempted)

-- | Private.
ruleOn :: String -> String -> ExemptionQuery
ruleOn rule moduleName =
  { rule
  , packageName: "here"
  , moduleName
  , path: ""
  , declarationName: Nothing
  }

-- | A finding in the module named, for `oldestFirst`.
-- | Private. Used only by `spec`.
findingIn :: String -> Located
findingIn moduleName =
  { packageName: "here"
  , moduleName
  , path: ""
  , finding:
      { rule: { name: RuleId "r", description: "", examples: Nothing }
      , groups: []
      , message: ""
      , hint: Nothing
      , declarationName: Nothing
      }
  }

-- | A root-file entry for every rule, over the modules named.
-- | Private.
exemptionOf :: Array String -> Kind -> String -> Exempt
exemptionOf modules kind since =
  { rule: "*"
  , modules
  , paths: []
  , kind
  , since
  , why: "because"
  , file: exemptFile
  , package: Nothing
  }

-- | Private.
recorded :: Exemptions
recorded =
  [ exemptionOf [ "Old" ] Backlog "2026-09-01"
  , exemptionOf [ "New" ] Backlog "2026-09-20"
  , exemptionOf [ "Undated" ] Backlog ""
  ]

-- | Private.
prefixed :: Exemptions
prefixed = [ exemptionOf [ "A.*" ] Backlog "" ]

-- | Private.
byPath :: Exemptions
byPath = [ (exemptionOf [] Backlog "") { paths = [ "Scratch.purs" ] } ]

-- | Private.
scoped :: Exemptions
scoped = [ (exemptionOf [ "A.B" ] Backlog "") { package = Just "here" } ]

-- | Private.
rules :: Array Rule
rules = [ RuleSet.rule (perDecl maxFunctionArity 0) ]

-- | Private.
dated :: String
dated =
  """
  exemptions:
    - rule: max-function-arity
      modules:
        - Fixture
      kind: backlog
      since: 2026-09-13
      why: proving the day is read
  """

-- | Private.
silencing :: String
silencing =
  """
  exemptions:
    - rule: max-function-arity
      modules:
        - Fixture
      kind: by-design
      why: proving the file is read
  """

-- | Private.
everythingHere :: String
everythingHere =
  """
  exemptions:
    - rule: max-function-arity
      modules:
        - "*"
      kind: by-design
      why: proving that * reaches as far as the file it is in
  """

-- | Private.
pendingOnly :: String
pendingOnly =
  """
  exemptions:
    - rule: max-function-arity
      modules:
        - Fixture
      kind: backlog
      why: proving a backlog entry is read
  """

-- ## Context
--
-- Not a unit test of the matcher alone: the question worth answering
-- is whether a file sitting in a workspace actually silences a rule,
-- and how far, and only a real run answers that. The workspace is the
-- fixture - two packages, one declaration with a binder each - so
-- every count above is small enough to say in words: two findings
-- with no file, one when one package is covered, none when both are.
--
-- A backlog entry is an exemption to everyone but the fixer, which is
-- what lets the nightly fixer see the backlog it is meant to work
-- through while every other run stays green.
