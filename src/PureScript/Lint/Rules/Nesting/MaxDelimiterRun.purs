module PureScript.Lint.Rules.Nesting.MaxDelimiterRun (maxDelimiterRun) where

import Prelude

import Data.Array (dropEnd, foldl, last, mapMaybe, snoc) as Array
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Maybe (maybe) as Maybe
import Data.String.Common (joinWith) as Str
import PureScript.CST.Range (tokensOf)
import PureScript.CST.Range.TokenList (toArray) as TokenList
import PureScript.CST.Types (Module, Token(..))
import PureScript.Lint.Rule (LintResult(..), ModuleLint, RuleName(..))

data Side = Opening | Closing

derive instance eqSide :: Eq Side

type Delimiter = { side :: Side, line :: Int }

type Run = { side :: Side, line :: Int, count :: Int }

-- | Uses `violations`.
maxDelimiterRun :: Int -> ModuleLint
maxDelimiterRun maxRun =
  { name: RuleName "max-delimiter-run"
  , description:
      "Flags more than the configured number of brackets opening or closing in immediate succession."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context cstModule -> case violations maxRun cstModule of
      [] -> Passed
      found -> Violation (Str.joinWith "; " found)
  }

-- | Private. Used only by `maxDelimiterRun`. Uses `tooLong`, `extendRun`, `delimiters`.
violations :: Int -> Module Void -> Array String
violations maxRun cstModule = Array.mapMaybe (tooLong maxRun)
  (Array.foldl extendRun [] (delimiters cstModule))

-- | Private, depth 2. Used only by `violations`. Uses `sideLabel`.
tooLong :: Int -> Run -> Maybe String
tooLong maxRun run =
  if run.count <= maxRun then Nothing
  else Just $ fold
    [ "line "
    , show (run.line + 1)
    , ": "
    , show run.count
    , " brackets "
    , sideLabel run.side
    , " in a row, over the max of "
    , show maxRun
    , " - use $ for the outermost application, or name an inner part with a let"
    ]

-- | Private, depth 2. Used only by `violations`. Uses `sideOf`.
delimiters :: Module Void -> Array Delimiter
delimiters cstModule = Array.mapMaybe
  (\token -> map (\side -> { side, line: token.range.start.line }) (sideOf token.value))
  (TokenList.toArray (tokensOf cstModule))

-- | Private, depth 3. Used only by `tooLong`.
sideLabel :: Side -> String
sideLabel = case _ of
  Opening -> "open"
  Closing -> "close"

-- | Private, depth 2. Used only by `violations`.
extendRun :: Array Run -> Delimiter -> Array Run
extendRun acc next =
  let
    fresh = Array.snoc acc { side: next.side, line: next.line, count: 1 }
    bumped run = Array.snoc (Array.dropEnd 1 acc) (run { count = run.count + 1 })
  in
    Maybe.maybe fresh (\run -> if run.side == next.side then bumped run else fresh)
      (Array.last acc)

-- | Private, depth 3. Used only by `delimiters`.
sideOf :: Token -> Maybe Side
sideOf = case _ of
  TokLeftParen -> Just Opening
  TokLeftSquare -> Just Opening
  TokRightParen -> Just Closing
  TokRightSquare -> Just Closing
  _ -> Nothing

-- ## Context
--
-- `)))`, `]))`, `(((`, `[((` - three or more brackets opening or
-- closing with nothing in between. Whitespace is irrelevant, which is
-- the point: the same structure formatted across three lines reads no
-- better than on one, so the rule counts adjacent delimiter *tokens*
-- rather than lines. The case that prompted it was the multi-line
-- form:
--
--         ]
--       )
--     )
--
-- Braces are excluded, parens and square brackets are not. A brace
-- delimits a record - data, read as one unit - so a record inside an
-- array inside a call is three different things each saying something.
-- Parens and squares delimit application and sequence, which is where
-- a genuine cascade comes from: `f (g (h (i x)))` is one thing wearing
-- four hats. Excluding braces is also what makes the rule affordable -
-- it removes 273 of 436 occurrences, nearly all of them the idiomatic
-- array-of-records shape.
--
-- Restricting further to parens alone was tried and rejected. It looks
-- defensible - purs-tidy hugs parens while giving squares and braces
-- an inner space, so an *unbroken* run really is only ever parens -
-- but that reasoning holds only on one line. The motivating case above
-- is `] ) )`, a square followed by two parens, which a parens-only
-- rule counts as two and lets through.
--
-- A run like this is the visible end of an expression that was built
-- inward instead of being named. The delimiters themselves carry no
-- information - by the time you reach the third one the interesting
-- part is well behind you - but you have to count them to work out what
-- just ended, and counting is exactly what a reader should never have
-- to do. Two fixes, in order of cheapness: `$` removes the outermost
-- pair outright (`f $ g x` for `f (g x)`), which often takes a run
-- below the limit on its own; and binding an inner part to a `let` and
-- passing its name shortens the run *and* gives the thing a word,
-- which is the better outcome when the inner part deserves one.
--
-- Deliberately *not* the same as `max-bracket-nesting-depth`, which
-- measures how deep nesting goes anywhere in a declaration. Nesting
-- that stays readable does not trip this rule; a run of closers does,
-- however shallow the nesting that produced it. Two rules, two
-- questions: how deep is this, and how hard is it to see where things
-- end.
--
-- Counted from token ranges rather than the file text, like
-- `max-line-length` and `max-string-literal-length`, so a comment or
-- string containing `)))` is not mistaken for structure.
