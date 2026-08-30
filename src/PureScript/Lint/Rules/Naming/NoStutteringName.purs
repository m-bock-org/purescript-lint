module PureScript.Lint.Rules.Naming.NoStutteringName (noStutteringName) where

import Prelude

import Data.Array (nub, null) as Array
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Maybe (isJust) as Maybe
import Data.String (Pattern(..))
import Data.String (joinWith, stripPrefix) as Str
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types
  ( Binder(..)
  , Expr(..)
  , Ident(..)
  , Module
  , ModuleName(..)
  , Operator(..)
  , Proper(..)
  , QualifiedName(..)
  , Type(..)
  )
import PureScript.Lint.Rule (LintResult(..), ModuleLint, RuleName(..))

-- | Uses `report`, `stutters`.
noStutteringName :: ModuleLint
noStutteringName =
  { name: RuleName "no-stuttering-name"
  , description:
      "Flags a qualified name whose own name repeats its qualifier, like `Kraken.KrakenPair`."
  , goodExample: Just "Kraken.Pair"
  , badExample: Just "Kraken.KrakenPair"
  , rule: \_context cstModule -> report (Array.nub (stutters cstModule))
  }

-- | Private. Used only by `noStutteringName`. Uses `fromExpr`, `fromType`, `fromBinder`.
stutters :: Module Void -> Array String
stutters = foldMapModule
  ( defaultMonoidalVisitor
      { onExpr = fromExpr, onType = fromType, onBinder = fromBinder }
  )

-- | Private, depth 2. Used only by `stutters`. Uses `stuttering`, `unIdent`, `unProper`,
-- | `unOperator`.
fromExpr :: Expr Void -> Array String
fromExpr = case _ of
  ExprIdent qn -> stuttering unIdent qn
  ExprConstructor qn -> stuttering unProper qn
  ExprOpName qn -> stuttering unOperator qn
  _ -> []

-- | Private, depth 2. Used only by `stutters`. Uses `stuttering`, `unProper`, `unOperator`.
fromType :: Type Void -> Array String
fromType = case _ of
  TypeConstructor qn -> stuttering unProper qn
  TypeOpName qn -> stuttering unOperator qn
  _ -> []

-- | Private, depth 2. Used only by `stutters`. Uses `stuttering`, `unProper`.
fromBinder :: Binder Void -> Array String
fromBinder = case _ of
  BinderConstructor qn _ -> stuttering unProper qn
  _ -> []

-- | Private.
stuttering :: ∀ a. (a -> String) -> QualifiedName a -> Array String
stuttering nameText (QualifiedName q) = case q.module of
  Just (ModuleName alias)
    | Maybe.isJust (Str.stripPrefix (Pattern alias) (nameText q.name)) ->
        [ alias <> "." <> nameText q.name ]
  _ -> []

-- | Private, depth 3. Used only by `fromExpr`.
unIdent :: Ident -> String
unIdent (Ident n) = n

-- | Private.
unProper :: Proper -> String
unProper (Proper n) = n

-- | Private.
unOperator :: Operator -> String
unOperator (Operator n) = n

-- | Private. Used only by `noStutteringName`.
report :: ∀ a. Array String -> LintResult a
report found
  | Array.null found = Passed
  | otherwise = Violation
      ( fold
          [ Str.joinWith ", " found
          , " repeats the qualifier in the name - drop the prefix from the name,"
          , " or import it unqualified"
          ]
      )

-- ## Context
--
-- `Kraken.KrakenPair` says "Kraken" twice, and `Map.Map` says it twice
-- over. The qualifier already tells a reader where the name comes from,
-- so repeating it in the name is noise at every use site - Go's standard
-- library calls this stuttering and bans it for the same reason.
--
-- Two fixes, and the message names both, because which one is right
-- depends on who owns the name. `Kraken.ApiUrl` was a rename inside
-- `api-kraken`, a package this repo owns. `Map.Map`, `Obj.Object` and
-- `NonNegativeDecimal.NonNegativeDecimal` are not renames at all: those
-- names are fine, and `require-qualified-imports` already lists each
-- type as an `except`, meaning it is meant to be imported unqualified.
-- This rule is what notices when that was not done.
--
-- Reads `QualifiedName`s out of the CST rather than the printed token
-- stream. A first attempt used tokens and split them on `.`, which
-- flagged every numeric literal in the repo - `0.0` looks exactly like a
-- qualifier repeating itself. The CST knows the difference between a
-- qualified name and a decimal point, and a rule this simple has no
-- excuse to guess.
--
-- Prefix rather than equality, so `Kraken.KrakenPair` and `Map.Map` are
-- both caught. Nothing else in this repo produces a false positive: the
-- one shape that could - an alias legitimately prefixing an unrelated
-- name - would be a module whose own types start with its alias, which
-- is the thing being banned.
