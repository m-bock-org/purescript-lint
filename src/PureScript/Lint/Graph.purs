module PureScript.Lint.Graph
  ( Graph
  , edgesOf
  , fromEdges
  , longestPath
  , successors
  , successorsOf
  ) where

import Prelude

import Data.Foldable (maximum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe) as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))

newtype Graph a = Graph (Map a (Set a))

fromEdges :: ∀ a. Ord a => Array (a /\ a) -> Graph a
fromEdges edges =
  let
    pairs = map (\(from /\ to) -> from /\ Set.singleton to) edges
  in
    Graph (Map.fromFoldableWith Set.union pairs)

successors :: ∀ a. Ord a => Graph a -> a -> Set a
successors (Graph edges) node = Maybe.fromMaybe Set.empty (Map.lookup node edges)

-- | Uses `successors`.
successorsOf :: ∀ a. Ord a => Graph a -> a -> Array a
successorsOf graph node = Set.toUnfoldable (successors graph node)

edgesOf :: ∀ a. Graph a -> Map a (Set a)
edgesOf (Graph edges) = edges

-- | Uses `longestPathFrom`.
longestPath :: ∀ a. Ord a => Graph a -> a -> Int
longestPath graph = longestPathFrom graph Set.empty

-- | Private. Used only by `longestPath`. Uses `successorsOf`.
longestPathFrom :: ∀ a. Ord a => Graph a -> Set a -> a -> Int
longestPathFrom graph visited node =
  let
    walked = Set.insert node visited
    hopTo callee =
      if Set.member callee walked then 0 else 1 + longestPathFrom graph walked callee
    callees = successorsOf graph node
  in
    Maybe.fromMaybe 0 (maximum (map hopTo callees))

-- ## Context
--
-- A small directed-graph module with just the operations this linter's
-- rules actually need - deliberately generic, knowing nothing about
-- CSTs, call graphs, or linting. Extracted (2026-08-28) when
-- `Lint.Rules.Nesting.MaxCallStackDepth` needed strongly-connected components:
-- a `Map k (Set k)` and an inline walk had been fine while the only
-- operation was "longest path", but SCC computation is a real algorithm
-- with real subtlety, and that is worth naming and isolating rather
-- than burying inside a rule.
--
-- `longestPath` counts *edges*, and treats an edge back into a node
-- already on the current path as free. That rule matters more than it
-- looks: charging a full hop for a self-call produced three immediate
-- false positives when this measure was first written
-- (`SameConstructorArm`'s `reconstructsConfiguredCtor` recursing through
-- `ExprParens`, among others). Re-entering something you are already
-- tracing adds nothing new to understand.
--
-- `condense` exists for the same reason one size up. Mutual recursion -
-- a recursive-descent AST walker being the canonical case - produces
-- genuinely long simple paths through a densely connected component,
-- but path length through a cycle is not a measure of how hard the code
-- is to follow: you read such a group as a set of cases that call back
-- into each other, not as a chain to trace end to end. Collapsing each
-- component to one node before measuring makes "how deep is this chain"
-- mean what it is supposed to mean, and is the exact generalisation of
-- the self-call rule above.
--
-- `componentsOf` uses Kosaraju rather than Tarjan - two straightforward
-- passes (finish-order on the forward graph, then reachability on the
-- reversed one) instead of one pass with an explicit lowlink stack.
-- Slower asymptotically by a constant, and much easier to read and to
-- convince yourself is correct, which is the right trade at the sizes a
-- single module's call graph reaches.
