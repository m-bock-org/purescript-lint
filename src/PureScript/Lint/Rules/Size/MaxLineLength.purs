module PureScript.Lint.Rules.Size.MaxLineLength (maxLineLength) where

import Prelude

import Data.Array (concatMap, filter, mapMaybe, range) as Array
import Data.Foldable (fold, foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String.Common (joinWith) as Str
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.CST.Range (rangeOf, tokensOf)
import PureScript.CST.Range.TokenList (toArray) as TokenList
import PureScript.CST.Types (Declaration(..), Module(..), ModuleBody(..), SourceToken, Token(..))
import PureScript.Lint.Rule (LintResult(..), ModuleLint, RuleName(..))

type LineWidths = { code :: Int, signature :: Int }

-- | Uses `overLongLines`.
maxLineLength :: LineWidths -> ModuleLint
maxLineLength widths =
  { name: RuleName "max-line-length"
  , description:
      "Flags a line over the configured width - a type signature gets its own, wider allowance than ordinary code."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context cstModule -> case overLongLines widths cstModule of
      [] -> Passed
      violations -> Violation (Str.joinWith "; " violations)
  }

-- | Private. Used only by `maxLineLength`. Uses `tooWide`, `signatureLines`, `widthByLine`.
overLongLines :: LineWidths -> Module Void -> Array String
overLongLines widths cstModule = Array.mapMaybe
  (tooWide widths (signatureLines cstModule))
  (Map.toUnfoldable (widthByLine cstModule))

-- | Private, depth 2. Used only by `overLongLines`.
tooWide :: LineWidths -> Set Int -> Int /\ Int -> Maybe String
tooWide widths sigLines (line /\ width) =
  let
    isSignature = Set.member line sigLines
    allowed = if isSignature then widths.signature else widths.code
    kind = if isSignature then " for a type signature" else " for code"
    message = fold
      [ "line "
      , show (line + 1)
      , " is "
      , show width
      , " columns, over the max of "
      , show allowed
      , kind
      ]
  in
    if width <= allowed then Nothing else Just message

-- | Private, depth 2. Used only by `overLongLines`. Uses `isStringToken`.
widthByLine :: Module Void -> Map Int Int
widthByLine cstModule =
  let
    widest acc token = Map.insertWith max token.range.end.line token.range.end.column acc
    tokens = TokenList.toArray (tokensOf cstModule)
  in
    foldl widest Map.empty (Array.filter (not <<< isStringToken) tokens)

-- | Private, depth 3. Used only by `widthByLine`.
isStringToken :: SourceToken -> Boolean
isStringToken token = case token.value of
  TokString _ _ -> true
  TokRawString _ -> true
  _ -> false

-- | Private, depth 2. Used only by `overLongLines`.
signatureLines :: Module Void -> Set Int
signatureLines (Module { body: ModuleBody { decls } }) =
  let
    spannedLines decl = case decl of
      DeclSignature _ -> Array.range (rangeOf decl).start.line (rangeOf decl).end.line
      DeclType _ _ _ -> Array.range (rangeOf decl).start.line (rangeOf decl).end.line
      _ -> []
  in
    Set.fromFoldable (Array.concatMap spannedLines decls)

-- ## Context
--
-- Two widths, not one: a type signature gets a wider allowance than the
-- code under it. A `type` synonym counts as a signature for this
-- purpose (added 2026-08-28): it is a type being read as one shape,
-- which is the whole reason the wider allowance exists, and the
-- omission only surfaced once decomposing long signatures started
-- moving their inline record types into named aliases - the alias
-- inherited the code width and was flagged for a line that had been
-- legal inside the signature it came from. The asymmetry is deliberate. A signature is read in a
-- different mode - you are taking in one continuous shape
-- (`a -> b -> c -> d`), not following logic - and scanning right across it
-- costs almost nothing, while stacking it vertically costs a lot, because
-- the shape stops being one thing you can see at once.
--
-- This rule exists because `purs-tidy` cannot express that distinction: it
-- has a single `width`, so the only way to stop it stacking long
-- signatures is to raise the width for everything. `.tidyrc.json` is
-- therefore set to the *signature* width and this rule holds ordinary code
-- to the narrower one - the formatter supplies the ceiling, the linter
-- supplies the floor. Note that purs-tidy breaks a signature that does not
-- fit but never re-joins one that would, so raising the width alone
-- changes nothing; the 91 already-stacked signatures needed a one-time
-- join, after which 73 stayed flat and 18 were re-broken as genuinely too
-- long.
--
-- Line widths come from token ranges rather than the raw file text, and
-- that is a deliberate constraint rather than an inconvenience. Every
-- other rule in this linter reads the CST, which means it sees the
-- *current* module including any fix an earlier rule applied. A rule
-- reading the on-disk text instead would silently measure a stale program
-- whenever a fix had been applied in the same pass, and would need runner
-- support to be ordered safely. Deriving widths from `tokensOf` keeps this
-- rule in the same representation as every other, with no ordering hazard
-- and no new rule category.
--
-- The cost of that choice: a line's width is where its last *token* ends,
-- so trailing comment text is not measured. Verified against
-- `CommentPolicy.purs` - every code line matched its real length exactly,
-- and the only divergences were the trailing prose block, which has no
-- tokens at all. Acceptable, and arguably correct: the concern here is
-- code width, and `comment-policy` already bans comments nearly
-- everywhere else.
--
-- String literals are excluded for the same reason. Measuring them flagged
-- 28 lines on the first run, every one a `description:` field or an
-- exemption's reason text - unbreakable single tokens no formatter can
-- split, whose length does not impede reading the structure around them.
-- Excluding them dropped the count to zero, which is the honest baseline:
-- this repo has no genuinely wide code, only wide prose. A line is
-- therefore judged by its widest *non-string* token, so
-- `f a b "...800 chars..."` is measured on `f a b`.
--
-- Line numbers from the CST are 0-based while editors are 1-based, hence
-- the `+ 1` when reporting. That off-by-one was caught by comparing
-- derived widths against real ones rather than by reading the types.
