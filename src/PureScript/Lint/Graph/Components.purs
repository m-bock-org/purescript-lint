module PureScript.Lint.Graph.Components
  ( componentsOf
  , condense
  ) where

import Prelude

import Data.Array (concatMap, cons, foldl, snoc, take, uncons) as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe) as Maybe
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import PureScript.Lint.Graph (Graph, edgesOf, fromEdges, successorsOf)

type Visit a = { seen :: Set a, order :: Array a }

type Assignment a = { assigned :: Set a, components :: Array (Set a) }

-- | Uses `nodesOf`, `visitForward`, `assign`, `reachable`.
componentsOf :: ∀ a. Ord a => Graph a -> Array (Set a)
componentsOf graph =
  let
    nodes = nodesOf graph

    incoming from = map (\to -> to /\ from) (successorsOf graph from)
    reversed = fromEdges (Array.concatMap incoming nodes)

    visited = Array.foldl (visitForward graph) { seen: Set.empty, order: [] } nodes

    grouped = Array.foldl
      ( \acc node ->
          if Set.member node acc.assigned then acc
          else assign acc (reachable reversed acc.assigned node)
      )
      { assigned: Set.empty, components: [] }
      visited.order
  in
    grouped.components

-- | Private, depth 2. Used only by `componentsOf`.
assign :: ∀ a. Ord a => Assignment a -> Set a -> Assignment a
assign acc component =
  { assigned: Set.union acc.assigned component
  , components: Array.snoc acc.components component
  }

-- | Private, depth 2. Used only by `componentsOf`.
visitForward :: ∀ a. Ord a => Graph a -> Visit a -> a -> Visit a
visitForward graph state node
  | Set.member node state.seen = state
  | otherwise =
      let
        marked = state { seen = Set.insert node state.seen }
        descended = Array.foldl (visitForward graph) marked (successorsOf graph node)
      in
        descended { order = Array.cons node descended.order }

-- | Private, depth 2. Used only by `componentsOf`. Uses `walkFrom`.
reachable :: ∀ a. Ord a => Graph a -> Set a -> a -> Set a
reachable graph excluded start = walkFrom { graph, excluded } [ start ] Set.empty

type Walk a = { graph :: Graph a, excluded :: Set a }

-- | Private, depth 3. Used only by `reachable`.
walkFrom :: ∀ a. Ord a => Walk a -> Array a -> Set a -> Set a
walkFrom ctx frontier found = Maybe.maybe found
  ( \{ head, tail } ->
      if Set.member head found || Set.member head ctx.excluded then walkFrom ctx tail found
      else walkFrom ctx (tail <> successorsOf ctx.graph head) (Set.insert head found)
  )
  (Array.uncons frontier)

-- | Private, depth 2. Used only by `componentsOf`.
nodesOf :: ∀ a. Ord a => Graph a -> Array a
nodesOf graph =
  let
    edges = edgesOf graph
    sources = Set.fromFoldable (Map.keys edges)
    targets = Set.unions (Map.values edges)
  in
    Set.toUnfoldable (Set.union sources targets)

-- | Uses `componentsOf`.
condense :: ∀ a. Ord a => Graph a -> { graph :: Graph a, representativeOf :: a -> a }
condense graph =
  let
    components = componentsOf graph

    representatives :: Map a a
    representatives = Map.fromFoldable do
      component <- components
      let members = Set.toUnfoldable component :: Array a
      leader <- Array.take 1 members
      map (\member -> member /\ leader) members

    representativeOf node = Maybe.fromMaybe node (Map.lookup node representatives)

    collapsed = do
      component <- components
      member <- Set.toUnfoldable component :: Array a
      successor <- successorsOf graph member
      let from = representativeOf member
      let to = representativeOf successor
      if from == to then [] else [ from /\ to ]
  in
    { graph: fromEdges collapsed, representativeOf }

-- ## Context
--
-- Strongly-connected components, and the condensation built from them,
-- split out of `PureScript.Lint.Graph` on 2026-08-29.
--
-- The split was recorded as needed a day earlier, after six attempts to
-- satisfy `max-delimiter-run` and `max-call-stack-depth` at once inside
-- one module - every bracket fix added a call hop and every depth fix
-- added brackets. That note turned out to be half right. The brackets
-- were fixable without any split: a `let` that only names a value is not
-- a call-graph node and is not what `hoistable-local` looks at, so
-- naming intermediate values is free on both axes, and every one of
-- those attempts had reached for a helper *function* instead.
--
-- The depth was not fixable that way. `condense` calls `componentsOf`,
-- which calls `reachable`, which walks - three hops before the walk does
-- anything, with the walk's own helper making four. Cross-module calls
-- cost nothing against the metric, so the boundary restarts the count,
-- and `condense` moves with `componentsOf` because it is that function's
-- only consumer and would otherwise close the cycle back into `Graph`.
--
-- Kosaraju rather than Tarjan: two straightforward passes (finish-order
-- on the forward graph, then reachability on the reversed one) instead
-- of one pass with an explicit lowlink stack. Slower by a constant, much
-- easier to convince yourself is correct, which is the right trade at
-- the sizes a single module's call graph reaches.
--
-- `condense` exists because path length through a cycle is not a measure
-- of how hard code is to follow. Mutual recursion - a recursive-descent
-- AST walker being the canonical case - produces genuinely long simple
-- paths through a densely connected component, but you read such a group
-- as a set of cases calling back into each other, not as a chain to
-- trace end to end. Collapsing each component to one node before
-- measuring makes "how deep is this chain" mean what it should.
