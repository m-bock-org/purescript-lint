-- | Fixes that are not a function of the parse tree.
-- |
-- | A rule with an autofix needs nobody: `runRules` applies it, in CI,
-- | and the finding never reaches a person. What is left over is the
-- | findings where detection is mechanical and the fix is a *name*, a
-- | *grouping* or a *sentence* - and those cannot be a pure rewrite,
-- | because there is nothing in the tree to derive them from.
-- |
-- | So a fix here is an **effect**: a function from a brief to new
-- | source. Nothing in this module knows or can find out what performs
-- | it. A model performs it; a person at a terminal could; a table
-- | performs it in a test. The linter's part is not to propose but to
-- | *judge* - and that is the whole safety argument, because a proposer
-- | that need not be trusted is a proposer that can be anything.
module Lint.Fix
  ( Brief
  , FixConfig
  , Guidance
  , Outcome(..)
  , Propose
  , guidanceFor
  , outcomeLine
  ) where

import Prelude

import Data.Array (find) as Array
import Data.Either (Either)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)

-- | What a proposer is told, and all it is told.
-- |
-- | The module's whole source rather than an excerpt, because a rule
-- | detects at one scope and is fixed at another: a finding on an
-- | expression is often fixed by naming part of it, which adds a
-- | declaration. Hand over the expression alone and the only fixes
-- | available are the ones that fit inside it - which are the ones not
-- | worth asking for.
-- |
-- | `broke` is what the previous attempt started, empty on the first.
-- | Restructuring earns new findings honestly: an extracted helper
-- | wants a declaration note, and a fix is not wrong for having added
-- | one. Telling the proposer what it started is what lets it finish.
type Brief =
  { rule :: String
  , moduleName :: String
  , path :: String
  , message :: String
  , guidance :: String
  , source :: String
  , broke :: Array String
  }

-- | Turning a brief into new source for that module.
-- |
-- | `Left` is the proposer declining, which is a first-class answer and
-- | not a failure - most findings, most of the time, are better left
-- | alone than guessed at.
type Propose = Brief -> Aff (Either String String)

-- | What a proposer should do about one rule.
-- |
-- | A rule with no entry is not fixed this way, which makes this table
-- | the switch at rule granularity. It is configuration rather than a
-- | field on the rule because it is a claim about one codebase's
-- | conventions, not about the style: what to do about `comment-policy`
-- | in a repository whose prose belongs in a trailing block is not what
-- | to do in one that has no such convention, and it is the same rule
-- | in both.
type Guidance = { rule :: String, says :: String }

-- | The whole of what a caller supplies to turn this on.
-- |
-- | Absent, nothing here runs and the linter behaves exactly as it did.
-- | That is the off switch, and it is one `Maybe` rather than a flag
-- | threaded through a second code path.
type FixConfig =
  { propose :: Propose
  , guidance :: Array Guidance
  -- | How many findings one run will attempt. Each costs a proposal
  -- | and a re-lint of the workspace, so this is a budget rather than a
  -- | safety limit.
  , limit :: Int
  -- | How many times a proposer may be told what it broke and try
  -- | again. Two is usually where a restructuring fix lands - the first
  -- | adds the declaration, the second gives it the note the style
  -- | wants.
  , rounds :: Int
  -- | A second, stronger acceptance test, run only once the re-lint is
  -- | already clean.
  -- |
  -- | Re-linting answers "is the finding gone, and did nothing else
  -- | start" - and a proposal can pass that while not compiling at
  -- | all. Rewriting `maybe' (\_ -> throw e) pure` as a case over
  -- | `Nothing`/`Just` is the example that forced this: every rule is
  -- | satisfied, and the module imported `Maybe` without its
  -- | constructors, so the result did not build. No rule notices,
  -- | because no rule is about that.
  -- |
  -- | What goes here is a typecheck. It is left to the caller rather
  -- | than done here because this module deliberately knows nothing
  -- | about how the code it lints is built, and a linter that shells
  -- | out to one build tool by name has picked one.
  -- |
  -- | Its `Left` is fed back as `broke`, so a failure is a round of
  -- | the retry loop rather than the end of the attempt: the proposer
  -- | is told what did not compile and gets to finish the job. Absent,
  -- | the re-lint is the only judge.
  , verify :: Maybe (Aff (Either String Unit))
  }

-- | What came of one finding.
data Outcome
  = Fixed
  | Declined String

derive instance Eq Outcome

-- | What to say about a rule, if anything.
guidanceFor :: Array Guidance -> String -> Maybe String
guidanceFor table rule = map _.says (Array.find (\g -> g.rule == rule) table)

-- | One line for the log.
outcomeLine :: String -> String -> Outcome -> String
outcomeLine rule moduleName = case _ of
  Fixed -> "fixed " <> rule <> " in " <> moduleName
  Declined why -> rule <> " in " <> moduleName <> " - " <> why
