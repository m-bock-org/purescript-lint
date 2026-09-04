-- | The fix loop, driven with no model anywhere.
-- |
-- | Which is the point of a fix being an effect: the proposer is a
-- | function, so a test supplies one that answers from a table and the
-- | whole judging path runs - write, lint again, keep or put back -
-- | with no network and nothing to mock.
module Test.Lint.FixSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Lint (runLinterWith)
import Lint.Internal.Exemptions as Exemptions
import Lint.Fix (guidanceFor, outcomeLine)
import Lint.Fix as Fix
import Lint.Rule (perDecl)
import Lint.RuleSet (Rule)
import Lint.RuleSet as RuleSet
import Test.Lint.ReadmeExample (maxFunctionArity)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = do
  describe "guidance" do
    it "is found by rule name" do
      guidanceFor table "max-function-arity" `shouldEqual` Just "give it fewer arguments"
    it "and a rule with no entry is not this service's to fix" do
      guidanceFor table "no-where-clauses" `shouldEqual` Nothing

  describe "outcomeLine" do
    it "says what was fixed" do
      outcomeLine "r" "M" Fix.Fixed `shouldEqual` "fixed r in M"
    it "and why it was not" do
      outcomeLine "r" "M" (Fix.Declined "no") `shouldEqual` "r in M - no"

  describe "fixWorkspace" do
    -- The proposal has to *differ* from the file, or this cannot tell
    -- a working revert from a missing one - handing back the source
    -- unchanged leaves the file identical either way, which is how the
    -- first version of this spec passed with the revert deleted.
    --
    -- Appending a comment changes the file and removes no finding, so
    -- the attempt must be declined and the file must come back exactly
    -- as it was.
    it "puts the source back when a proposal is rejected" do
      -- The file to check is the one actually proposed to, which is
      -- whichever module the first finding is in - not one named here.
      -- Naming one meant asserting that an untouched file was
      -- untouched, and that passed with the revert deleted.
      seen <- liftEffect (Ref.new Nothing)
      _ <- runLinterWith
        { skipModules: []
        , standing: Exemptions.All
        , fix: Just
            { propose: \brief -> do
                liftEffect (Ref.write (Just { path: brief.path, source: brief.source }) seen)
                pure (Right (brief.source <> "\n-- a proposal that fixes nothing\n"))
            , guidance: table
            , limit: 1
            , rounds: 1
            , verify: Nothing
            }
        }
        rules
      touched <- liftEffect (Ref.read seen)
      case touched of
        Nothing -> fail "nothing was proposed to, so this spec checked nothing"
        Just { path, source } -> do
          after <- FS.readTextFile UTF8 path
          after `shouldEqual` source

    it "asks nobody when no rule has guidance" do
      asked <- liftEffect (Ref.new 0)
      _ <- runLinterWith
        { skipModules: []
        , standing: Exemptions.All
        , fix: Just
            { propose: \_ -> do
                liftEffect (Ref.modify_ (_ + 1) asked)
                pure (Left "should never be called")
            , guidance: []
            , limit: 10
            , rounds: 1
            , verify: Nothing
            }
        }
        rules
      count <- liftEffect (Ref.read asked)
      count `shouldEqual` 0

    -- An empty module has no declarations, so every finding in it is
    -- gone and the re-lint is perfectly happy - which is the whole
    -- shape this guards against. Lint cannot see that the rest of the
    -- workspace no longer compiles, because no rule is about that.
    it "a proposal that lints clean but fails verify is put back, and the proposer is told why" do
      rounds <- liftEffect (Ref.new [])
      proposed <- liftEffect (Ref.new Nothing)
      _ <- runLinterWith
        { skipModules: []
        , standing: Exemptions.All
        , fix: Just
            { propose: \brief -> do
                let emptied = "module " <> brief.moduleName <> " where\n"
                liftEffect (Ref.modify_ (_ <> [ brief.broke ]) rounds)
                liftEffect (Ref.write (Just { path: brief.path, emptied }) proposed)
                pure (Right emptied)
            , guidance: table
            , limit: 1
            , rounds: 2
            , verify: Just (pure (Left "UnknownName: Nothing"))
            }
        }
        rules

      -- Told what did not compile, and asked again - a failed verify is
      -- a round of the retry loop, not the end of the attempt.
      asked <- liftEffect (Ref.read rounds)
      asked `shouldEqual` [ [], [ "UnknownName: Nothing" ] ]

      -- And the file is not left emptied.
      touched <- liftEffect (Ref.read proposed)
      case touched of
        Nothing -> fail "nothing was proposed to, so this proved nothing"
        Just { path, emptied } -> do
          now <- FS.readTextFile UTF8 path
          when (now == emptied) (fail (path <> " was left as the rejected proposal"))

    it "a proposer that declines changes nothing" do
      before <- FS.readTextFile UTF8 thisFile
      _ <- runLinterWith
        { skipModules: []
        , standing: Exemptions.All
        , fix: Just
            { propose: \_ -> pure (Left "not today")
            , guidance: table
            , limit: 1
            , rounds: 2
            , verify: Nothing
            }
        }
        rules
      after <- FS.readTextFile UTF8 thisFile
      after `shouldEqual` before

-- | Private.
table :: Array Fix.Guidance
table = [ { rule: "max-function-arity", says: "give it fewer arguments" } ]

-- | attempt.
-- |
-- | An empty set made every spec here pass without asking anybody
-- | anything - which they did, until this was noticed. `max-function-arity`
-- | at zero fires on any declaration with a binder, so this repository
-- | always has findings and the judging path always runs.
-- | Private.
rules :: Array Rule
rules = [ RuleSet.rule (perDecl maxFunctionArity 0) ]

-- | Private.
thisFile :: String
thisFile = "test/Test/Lint/FixSpec.purs"


