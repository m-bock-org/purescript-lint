module PureScript.Lint.Rules.Branches.SameConstructorArm (SameConstructorArmConfig, sameConstructorArm) where

import Prelude

import Data.Foldable (any, fold)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.CST.Types
  ( Binder(..)
  , Expr(..)
  , Guarded(..)
  , Proper(..)
  , QualifiedName(..)
  , Separated(..)
  , Where(..)
  , Wrapped(..)
  )
import PureScript.Lint.Rule (ExprLint, LintResult(..), RuleName(..))

type SameConstructorArmConfig = { ctor :: String }

-- | Uses `isSelfReconstructingArm`.
sameConstructorArm :: SameConstructorArmConfig -> ExprLint
sameConstructorArm config =
  { name: RuleName "same-constructor-arm"
  , description:
      "Flags a one-argument-constructor case arm that reconstructs the same constructor."
  , goodExample: Just "do x <- m\n   pure (f x)"
  , badExample: Just "case m of\n  Just x -> Just (f x)\n  Nothing -> Nothing"
  , rule: \_context expr -> case expr of
      ExprCase { branches } | any (isSelfReconstructingArm config) branches ->
        Violation
          ( fold
              [ "a branch matching "
              , config.ctor
              , " reconstructs "
              , config.ctor
              , " around its result - consider map/lmap/rmap/bimap, or do-notation"
              ]
          )
      _ -> Passed
  }

type CaseBranch = Separated (Binder Void) /\ Guarded Void

-- | Private. Used only by `sameConstructorArm`. Uses `isConfiguredBinder`,
-- | `unconditionalResultReconstructs`.
isSelfReconstructingArm :: SameConstructorArmConfig -> CaseBranch -> Boolean
isSelfReconstructingArm config (Separated { head } /\ guarded) =
  isConfiguredBinder config head && unconditionalResultReconstructs config guarded

-- | Private, depth 2. Used only by `isSelfReconstructingArm`.
isConfiguredBinder :: SameConstructorArmConfig -> Binder Void -> Boolean
isConfiguredBinder config = case _ of
  BinderConstructor (QualifiedName { name: Proper n }) [ _ ] -> n == config.ctor
  _ -> false

-- | Private, depth 2. Used only by `isSelfReconstructingArm`. Uses `reconstructsConfiguredCtor`.
unconditionalResultReconstructs :: SameConstructorArmConfig -> Guarded Void -> Boolean
unconditionalResultReconstructs config = case _ of
  Unconditional _ (Where { bindings: Nothing, expr }) -> reconstructsConfiguredCtor config expr
  _ -> false

-- | Private, depth 3. Used only by `unconditionalResultReconstructs`.
reconstructsConfiguredCtor :: SameConstructorArmConfig -> Expr Void -> Boolean
reconstructsConfiguredCtor config = case _ of
  ExprApp (ExprConstructor (QualifiedName { name: Proper n })) _ -> n == config.ctor
  ExprParens (Wrapped { value }) -> reconstructsConfiguredCtor config value
  _ -> false

-- Context: `sameConstructorArm` is one generic rule, configured once
-- per one-argument constructor it applies to (`{ ctor: "Left" }`,
-- `{ ctor: "Right" }`, `{ ctor: "Just" }`) - see
-- `PureScript.Lint.Rules.Naming.SingleExport`'s own note on this repo's "one rule, N
-- configured applications" convention. Deliberately name-based, not
-- type-based - this linter has no type information, so it matches
-- `Left`/`Right`/`Just` purely as CST constructor names, the same
-- limitation `PureScript.Lint.Rules.Naming.EncodeDecodeSignature`/
-- `PureScript.Lint.Rules.Idiom.RequireTupleOperator` already accept for their own
-- name-based heuristics.
--
-- Only handles `Unconditional` branches (no guards) - a guarded
-- branch's cross-outcome shape is `PureScript.Lint.Rules.Branches.GuardedConstructorBranch`'s
-- concern instead, kept separate rather than folded in here, since
-- this rule only ever needs to look at one arm in isolation to decide
-- whether it fires; the guarded shape genuinely needs to reason about
-- multiple outcomes to say anything precise (see that rule's own
-- comment). Also skips a branch with local `where`-bindings
-- (`bindings: Just _`) - a branch that needs its own local definitions
-- is doing more than "reconstruct the same constructor," so it's
-- outside this rule's narrow, purely-syntactic scope on purpose.
--
-- The hint is deliberately broad ("map/lmap/rmap/bimap, or
-- do-notation") rather than naming one specific fix - this rule sees
-- only one arm, so it genuinely can't tell from here alone whether the
-- case as a whole wants `lmap` (other arm untouched), half of `bimap`
-- (other arm also transforms), or a do-notation bind (other arm
-- continues the computation) - that's real information this rule
-- doesn't have, not a hedge.
