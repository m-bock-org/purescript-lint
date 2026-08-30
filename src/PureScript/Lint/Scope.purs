module PureScript.Lint.Scope
  ( BindingId(..)
  , Reference
  , bindingName
  , moduleReferences
  ) where

import Prelude

import Data.Array (concatMap, cons, foldl, head, mapMaybe, null) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (toArray) as NEA
import Data.Const (Const(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe (maybe) as Maybe
import Data.Newtype (un)
import Data.Tuple (Tuple(..))
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapBinder, traverseExpr)
import PureScript.CST.Types
  ( Binder(..)
  , Declaration(..)
  , DoStatement(..)
  , Expr(..)
  , Guarded(..)
  , GuardedExpr(..)
  , Ident(..)
  , Labeled(..)
  , LetBinding(..)
  , Module(..)
  , ModuleBody(..)
  , Name(..)
  , PatternGuard(..)
  , QualifiedName(..)
  , Separated(..)
  , Type(..)
  , ValueBindingFields
  , Where(..)
  , Wrapped(..)
  )

data BindingId
  = TopLevelBinding String
  | LocalBinding { name :: String, line :: Int, column :: Int }

derive instance eqBindingId :: Eq BindingId
derive instance ordBindingId :: Ord BindingId

type Reference = { from :: BindingId, to :: BindingId }

type Scope = Map String BindingId

bindingName :: BindingId -> String
bindingName = case _ of
  TopLevelBinding n -> n
  LocalBinding { name } -> name

-- | Uses `callableDeclName`, `valueBindingReferences`, `identName`.
moduleReferences :: Module Void -> Array Reference
moduleReferences (Module { body: ModuleBody { decls } }) =
  let
    callables = Array.mapMaybe (callableDeclName decls) decls

    topLevelScope :: Scope
    topLevelScope = Map.fromFoldable (map (\n -> n /\ TopLevelBinding n) callables)
  in
    Array.concatMap
      ( case _ of
          DeclValue fields ->
            valueBindingReferences topLevelScope (TopLevelBinding $ identName fields.name)
              fields
          _ -> []
      )
      decls

-- | Private. Used only by `moduleReferences`. Uses `callableName`.
callableDeclName :: Array (Declaration Void) -> Declaration Void -> Maybe String
callableDeclName decls = case _ of
  DeclValue fields -> callableName decls fields
  _ -> Nothing

-- | Private, depth 2. Used only by `callableDeclName`. Uses `identName`, `isCallable`,
-- | `isFunctionType`, `signatureOf`.
callableName :: Array (Declaration Void) -> ValueBindingFields Void -> Maybe String
callableName decls fields =
  let
    name = identName fields.name
    callable = isCallable fields || Maybe.maybe true isFunctionType (signatureOf decls name)
  in
    if callable then Just name else Nothing

-- | Private, depth 3. Used only by `callableName`.
signatureOf :: Array (Declaration Void) -> String -> Maybe (Type Void)
signatureOf decls name = Array.head $ Array.mapMaybe
  ( case _ of
      DeclSignature (Labeled { label: Name { name: Ident n }, value }) ->
        if n == name then Just value else Nothing
      _ -> Nothing
  )
  decls

-- | Private, depth 3. Used only by `callableName`.
isFunctionType :: Type Void -> Boolean
isFunctionType = case _ of
  TypeArrow _ _ _ -> true
  TypeForall _ _ _ t -> isFunctionType t
  TypeConstrained _ _ t -> isFunctionType t
  TypeKinded t _ _ -> isFunctionType t
  TypeParens (Wrapped { value }) -> isFunctionType value
  _ -> false

-- | Private.
identName :: Name Ident -> String
identName (Name { name: Ident n }) = n

-- | Private.
localIdOf :: Name Ident -> BindingId
localIdOf (Name { token, name: Ident n }) = LocalBinding
  { name: n, line: token.range.start.line, column: token.range.start.column }

-- | Private.
binderNames :: Binder Void -> Array String
binderNames = foldMapBinder
  ( defaultMonoidalVisitor
      { onBinder = case _ of
          BinderVar (Name { name: Ident n }) -> [ n ]
          BinderNamed (Name { name: Ident n }) _ _ -> [ n ]
          _ -> []
      }
  )

-- | Private. Used only by `exprReferences`. Uses `binderNames`.
lambdaNames :: NonEmptyArray (Binder Void) -> Array String
lambdaNames binders = Array.concatMap binderNames (NEA.toArray binders)

-- | Private.
shadowAll :: Array String -> Scope -> Scope
shadowAll names scope = Array.foldl (flip Map.delete) scope names

-- | Private. Uses `guardedReferences`, `shadowAll`, `binderNames`.
valueBindingReferences :: Scope -> BindingId -> ValueBindingFields Void -> Array Reference
valueBindingReferences scope current fields =
  guardedReferences (shadowAll (Array.concatMap binderNames fields.binders) scope)
    current
    fields.guarded

-- | Private. Used only by `guardedReferences`. Uses `patternGuardScope`, `patternGuardRefs`,
-- | `whereReferences`.
guardedExprReferences :: Scope -> BindingId -> GuardedExpr Void -> Array Reference
guardedExprReferences scope current (GuardedExpr g) =
  let
    Separated { head, tail } = g.patterns
    step acc pg =
      { scope: patternGuardScope acc.scope pg
      , refs: acc.refs <> patternGuardRefs current acc.scope pg
      }
    seeded = Array.foldl step { scope, refs: [] } (Array.cons head (map Tuple.snd tail))
  in
    seeded.refs <> whereReferences seeded.scope current g.where

-- | Private, depth 2. Used only by `guardedExprReferences`. Uses `exprReferences`.
patternGuardRefs :: BindingId -> Scope -> PatternGuard Void -> Array Reference
patternGuardRefs current sc (PatternGuard { expr }) = exprReferences sc current expr

-- | Private, depth 2. Used only by `guardedExprReferences`. Uses `shadowAll`, `binderNames`.
patternGuardScope :: Scope -> PatternGuard Void -> Scope
patternGuardScope sc (PatternGuard { binder }) =
  Maybe.maybe sc (\(Tuple b _) -> shadowAll (binderNames b) sc) binder

-- | Private. Uses `whereReferences`, `guardedExprReferences`.
guardedReferences :: Scope -> BindingId -> Guarded Void -> Array Reference
guardedReferences scope current =
  case _ of
    Unconditional _ w -> whereReferences scope current w
    Guarded ges -> Array.concatMap (guardedExprReferences scope current) (NEA.toArray ges)

-- | Private. Uses `exprReferences`, `extendWithLetBindings`, `letBindingReferences`.
whereReferences :: Scope -> BindingId -> Where Void -> Array Reference
whereReferences scope current (Where { expr, bindings }) = Maybe.maybe
  (exprReferences scope current expr)
  ( \(_ /\ bs) ->
      let
        inner = extendWithLetBindings scope (NEA.toArray bs)
      in
        letBindingReferences inner current (NEA.toArray bs) <> exprReferences inner current expr
  )
  bindings

-- | Private.
isCallable :: ValueBindingFields Void -> Boolean
isCallable fields = not (Array.null fields.binders)

-- | Private. Uses `isCallable`, `identName`, `localIdOf`, `shadowAll`, `binderNames`.
extendWithLetBindings :: Scope -> Array (LetBinding Void) -> Scope
extendWithLetBindings = Array.foldl
  ( \scope -> case _ of
      LetBindingName fields
        | isCallable fields -> Map.insert (identName fields.name) (localIdOf fields.name) scope
        | otherwise -> Map.delete (identName fields.name) scope
      LetBindingPattern binder _ _ -> shadowAll (binderNames binder) scope
      _ -> scope
  )

-- | Private. Uses `isCallable`, `valueBindingReferences`, `localIdOf`, `whereReferences`.
letBindingReferences :: Scope -> BindingId -> Array (LetBinding Void) -> Array Reference
letBindingReferences scope current = Array.concatMap case _ of
  LetBindingName fields
    | isCallable fields -> valueBindingReferences scope (localIdOf fields.name) fields
    | otherwise -> valueBindingReferences scope current fields
  LetBindingPattern _ _ w -> whereReferences scope current w
  _ -> []

type CaseBranch = Separated (Binder Void) /\ Guarded Void

-- | Private. Used only by `exprReferences`. Uses `binderNames`, `guardedReferences`, `shadowAll`.
branchReferences :: Scope -> BindingId -> CaseBranch -> Array Reference
branchReferences scope current (Separated { head: b, tail: bt } /\ guarded) =
  let
    names = Array.concatMap binderNames (Array.cons b (map Tuple.snd bt))
  in
    guardedReferences (shadowAll names scope) current guarded

-- | Private. Uses `extendWithLetBindings`, `letBindingReferences`, `shadowAll`, `lambdaNames`,
-- | `branchReferences`, `doReferences`.
exprReferences :: Scope -> BindingId -> Expr Void -> Array Reference
exprReferences scope current expr = case expr of
  ExprIdent (QualifiedName { module: Nothing, name: Ident n }) ->
    Maybe.maybe [] (\to -> [ { from: current, to } ]) (Map.lookup n scope)

  ExprLet { bindings, body } ->
    let
      inner = extendWithLetBindings scope (NEA.toArray bindings)
    in
      letBindingReferences inner current (NEA.toArray bindings)
        <> exprReferences inner current body

  ExprLambda { binders, body } ->
    exprReferences (shadowAll (lambdaNames binders) scope) current body

  ExprCase { head, branches } ->
    let
      Separated { head: h, tail: t } = head
      headRefs = Array.concatMap (exprReferences scope current)
        (Array.cons h (map Tuple.snd t))
    in
      headRefs <> Array.concatMap (branchReferences scope current) (NEA.toArray branches)

  ExprDo { statements } -> doReferences scope current (NEA.toArray statements)

  other -> un Const
    ( traverseExpr
        { onExpr: \e -> Const (exprReferences scope current e)
        , onBinder: pure
        , onType: pure
        }
        other
    )

-- | Private. Used only by `exprReferences`. Uses `doStatementStep`.
doReferences :: Scope -> BindingId -> Array (DoStatement Void) -> Array Reference
doReferences scope current statements =
  (Array.foldl (doStatementStep current) { scope, refs: [] } statements).refs

type ScopeAcc = { scope :: Scope, refs :: Array Reference }

-- | Private, depth 2. Used only by `doReferences`. Uses `exprReferences`, `shadowAll`,
-- | `binderNames`, `letStatementStep`.
doStatementStep :: BindingId -> ScopeAcc -> DoStatement Void -> ScopeAcc
doStatementStep current acc = case _ of
  DoDiscard e -> acc { refs = acc.refs <> exprReferences acc.scope current e }
  DoBind binder _ e -> acc
    { scope = shadowAll (binderNames binder) acc.scope
    , refs = acc.refs <> exprReferences acc.scope current e
    }
  DoLet _ bs -> letStatementStep current acc (NEA.toArray bs)
  _ -> acc

-- | Private, depth 3. Used only by `doStatementStep`. Uses `extendWithLetBindings`,
-- | `letBindingReferences`.
letStatementStep :: BindingId -> ScopeAcc -> Array (LetBinding Void) -> ScopeAcc
letStatementStep current acc bs =
  let
    inner = extendWithLetBindings acc.scope bs
  in
    acc { scope = inner, refs = acc.refs <> letBindingReferences inner current bs }

-- ## Context
--
-- A scope-correct resolver from every identifier *reference* in a module
-- to the *binding* it actually refers to - deliberately generic CST
-- machinery, not tied to any one rule. `Lint.Rules.Nesting.MaxCallStackDepth` is
-- its first consumer (it folds the references into a call graph), but
-- nothing here knows about call graphs, depth, or linting; an
-- unused-binding or accidental-shadowing rule would use the same output.
--
-- Exists because name-based matching is wrong in two ways that both
-- showed up live (2026-08-28). First, a *parameter* named the same as a
-- `let`/`where` binding elsewhere in the same declaration produced a
-- phantom edge - `SingleExport`'s `wrongExportsMessage moduleName`
-- appeared to "call" the unrelated `let`-bound `moduleName`, inflating
-- its reported chain by two hops. Second, two different `let` blocks in
-- one declaration that each bind the same name (`go`, `step`, ...) were
-- indistinguishable, silently merging two unrelated bindings into one
-- node. `BindingId` fixes the second by identifying a local binding by
-- its own source position rather than its name; the scope threading
-- fixes the first.
--
-- `Scope` is one flat `Map String BindingId` seeded with every top-level
-- name in the module, and shadowing is *deletion* from it. That's what
-- lets a parameter correctly shadow both a local binding and a
-- same-named top-level function - a design with a separate "locals" map
-- and a top-level fallback set can't express that, which is exactly the
-- bug this replaces. Names bound by something that isn't itself a
-- referenceable binding (function parameters, lambda binders, case
-- binders, `do` binds, pattern `let`s) are deleted rather than inserted:
-- a reference to one resolves to no binding at all, which is correct -
-- it names a value, not a definition anything can call.
--
-- Scoping is precise rather than approximate in the places where the
-- two differ: `case` binders scope to their own branch only (not to
-- sibling branches or the scrutinee), `do` binds scope only to
-- statements *after* them (`doReferences` threads left to right), and
-- `let`/`where` bindings are mutually visible before any right-hand side
-- is walked, matching PureScript's actual recursive-let semantics.
-- `valueBindingReferences` shadows a binding's own parameters within its
-- body, then walks its right-hand side with `current` set to that
-- binding, so references found there are attributed to it rather than to
-- whatever encloses it.
--
-- None of this is redundant with PureScript's own `ShadowedName`
-- warning, which is on by default and does fire in this repo. That
-- warning catches an inner binder hiding an *in-scope* outer name; the
-- bug that motivated this module was the opposite shape - two bindings
-- in *disjoint* scopes that merely share a spelling (`SingleExport`'s
-- `wrongExportsMessage moduleName` parameter versus an unrelated
-- `let`-bound `moduleName` inside a sibling field's lambda). Nothing is
-- shadowed there, the compiler is right to stay quiet, and only real
-- scope resolution can tell the two apart. The shadowing handling here
-- earns its place separately, since shadowing is warned but permitted.
--
-- The traversal leans on `PureScript.CST.Traversal.traverseExpr`, which
-- visits an expression's *immediate* children only. That's what keeps
-- this small: only the constructors that bind or scope something need
-- handling by hand, and every other constructor falls through to one
-- generic line. `Const (Array Reference)` supplies the accumulating
-- Applicative that traversal needs, so nothing is rebuilt.
--
-- Only `DeclValue` declarations are walked - a class method signature or
-- data declaration has no expression body to find references in.
--
-- A binding with no parameters is a *value*, not a node: reading it is a
-- lookup, not a call, so it neither costs a hop nor appears in the
-- graph, and its own references pass through to whatever encloses it.
-- Locally that is decided by `isCallable` alone. At top level the
-- binder count is not enough, because a point-free definition
-- (`rawComment = case _ of ...`) has no binders and is unmistakably a
-- function - 36 of them in this repo would have been misread as values.
-- The type signature settles it: no binders *and* a non-function type
-- means a value. Without that distinction every module paid a hop for
-- reading its own `tag = Tag "trade"` constant, which made the rule
-- measure something other than call depth.
