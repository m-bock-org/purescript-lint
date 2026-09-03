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
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Lint (runLinterWith)
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
        , fix: Just
            { propose: \brief -> do
                liftEffect (Ref.write (Just { path: brief.path, source: brief.source }) seen)
                pure (Right (brief.source <> "\n-- a proposal that fixes nothing\n"))
            , guidance: table
            , limit: 1
            , rounds: 1
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
        , fix: Just
            { propose: \_ -> do
                liftEffect (Ref.modify_ (_ + 1) asked)
                pure (Left "should never be called")
            , guidance: []
            , limit: 10
            , rounds: 1
            }
        }
        rules
      count <- liftEffect (Ref.read asked)
      count `shouldEqual` 0

    it "a proposer that declines changes nothing" do
      before <- FS.readTextFile UTF8 thisFile
      _ <- runLinterWith
        { skipModules: []
        , fix: Just
            { propose: \_ -> pure (Left "not today")
            , guidance: table
            , limit: 1
            , rounds: 2
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


