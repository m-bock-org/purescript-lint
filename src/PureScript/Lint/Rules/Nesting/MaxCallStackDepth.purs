module PureScript.Lint.Rules.Nesting.MaxCallStackDepth (maxCallStackDepth) where

import Prelude

import Data.Array (mapMaybe) as Array
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.String.Common (joinWith) as Str
import Data.Tuple.Nested ((/\))
import PureScript.CST.Types (Declaration(..), Ident(..), Module(..), ModuleBody(..), Name(..))
import PureScript.Lint.Graph (Graph, fromEdges, longestPath)
import PureScript.Lint.Graph.Components (condense)
import PureScript.Lint.Rule (LintResult(..), ModuleLint, RuleName(..))
import PureScript.Lint.Scope (BindingId(..), moduleReferences)

-- | Uses `violations`.
maxCallStackDepth :: Int -> ModuleLint
maxCallStackDepth maxHops =
  { name: RuleName "max-call-stack-depth"
  , description:
      "Flags a function whose longest local call chain exceeds the configured number of hops."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context cstModule -> case violations maxHops cstModule of
      [] -> Passed
      vs -> Violation (Str.joinWith "; " vs)
  }

-- | Private. Used only by `maxCallStackDepth`. Uses `checkHops`, `topLevelName`.
violations :: Int -> Module Void -> Array String
violations maxHops cstModule@(Module { body: ModuleBody { decls } }) =
  let
    edges = map (\r -> r.from /\ r.to) (moduleReferences cstModule)
    condensed = condense (fromEdges edges)
  in
    Array.mapMaybe (checkHops maxHops condensed) (Array.mapMaybe topLevelName decls)

type Condensed = { graph :: Graph BindingId, representativeOf :: BindingId -> BindingId }

-- | Private, depth 2. Used only by `violations`.
checkHops :: Int -> Condensed -> String -> Maybe String
checkHops maxHops condensed name =
  let
    n = longestPath condensed.graph (condensed.representativeOf (TopLevelBinding name))
    message = fold
      [ name, " is ", show n, " local call hops deep, over the max of ", show maxHops ]
  in
    if n > maxHops then Just message else Nothing

-- | Private, depth 2. Used only by `violations`.
topLevelName :: Declaration Void -> Maybe String
topLevelName = case _ of
  DeclValue { name: Name { name: Ident n } } -> Just n
  _ -> Nothing

-- Context: flags any function whose longest local call chain takes more
-- than `maxHops` hops to walk. The graph itself - which reference means
-- which binding - comes from `PureScript.Lint.Scope`, which resolves every
-- identifier against real lexical scope; this module only folds those
-- references into a `Map` and measures the longest path. That split is
-- deliberate: scope resolution is generic CST machinery with no opinion
-- about call graphs, and it earned its own module the moment name-based
-- matching produced its first phantom edge (see that module's own note
-- for the two bugs that motivated it).
--
-- Counts *edges*, not nodes (2026-08-28): a function that calls nothing
-- local is 0 hops, `f -> g` is 1, `f -> g -> h` is 2. The original
-- counted nodes instead (a leaf was 1), which was inherited rather than
-- chosen and never re-examined until the user pointed out it reads
-- backwards - "hops" are the steps *between* functions, so a function
-- that makes no call has made no hops. The practical cost of the old
-- convention was that `maxCallStackDepth 4` actually permitted only 3
-- calls deep, an off-by-one every reader had to carry. `3` is the
-- configured max in `Lint.purs`, exactly equivalent to the old `4`.
-- 3 is also where the user landed independently on readability grounds:
-- a three-hop chain is still traceable in your head, four is where it
-- starts to slip.
--
-- An edge back into a function already on the current path costs 0
-- hops, not 1 (`hopTo`'s own guard, not just a base case). That
-- distinction only shows up under edge-counting and is easy to get
-- wrong: the first version of this flip charged a full hop for a
-- self-call, which immediately produced three false positives
-- (`SameConstructorArm`'s `reconstructsConfiguredCtor` recursing through
-- `ExprParens`, and two like it) that the node-counting version had
-- never flagged. Recursing into a function you are already tracing
-- doesn't add another function to understand - the cycle is one thing
-- to hold in mind, however many times it goes around - so it
-- contributes nothing.
--
-- Where- *and* let-bound helpers are graph nodes, not just top-level
-- declarations (`where`, 2026-08-28; `let`, same day, closing a second
-- instance of the identical gap): folding a helper into either doesn't
-- shorten the actual chain a reader has to trace to understand the
-- caller, it just deletes that helper as a separately-counted node. The
-- two are the same construct with different surface syntax - `f = body
-- where g = ...` and `f = let g = ... in body` read identically to a
-- reader tracing `f`'s logic - so a rule that counted one and not the
-- other would just move the escape hatch, which is exactly what
-- happened when this module first shipped `where`-awareness alone:
-- several "fixes" landed the same day by relocating a chain into `let`
-- instead, dropping the reported number without shortening anything
-- real. Counting both closes that without discouraging the style
-- itself, which is still right when a helper genuinely has one caller
-- (it signals "private to this one function," shrinks the module's real
-- top-level surface, and keeps the helper next to its only caller) -
-- the point is that the *count* should reflect the true chain
-- regardless of which syntax expresses it, not that nesting is wrong.
--
-- Does not treat `env.foo`-style record-field projections through a
-- capability parameter as local calls - `PureScript.Lint.Scope` only
-- resolves bare identifiers, and a record accessor is a different CST
-- shape entirely. That's deliberate: a dependency threaded through an
-- explicit parameter is architecturally different from a same-module
-- function hard-wiring a call to another by name - the caller's own
-- signature documents it, it's independently swappable and testable,
-- and there's no hidden same-module graph to trace. Where a violation's
-- real cause is "this function reaches for some capability," injecting
-- it (the `Env` pattern this codebase already leans on) is the fix.
--
-- No memoization of `hopsFrom` across repeated exploration of the same
-- node via different paths - same complexity characteristic the
-- original had, not a regression, and no module here has made it a real
-- cost.
