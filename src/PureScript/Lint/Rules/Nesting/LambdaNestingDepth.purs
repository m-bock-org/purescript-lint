module PureScript.Lint.Rules.Nesting.LambdaNestingDepth (maxLambdaNestingDepth) where

import Prelude

import Control.Monad.State (State, execState, modify_)
import Data.Maybe (Maybe(..))
import Data.String.Common (joinWith) as Str
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.CST.Traversal (defaultVisitorWithContextM, rewriteDeclWithContextM)
import PureScript.CST.Types (Declaration, Expr(..))
import PureScript.Lint.Rule (DeclarationLint, LintResult(..), RuleName(..))

-- | Uses `violations`.
maxLambdaNestingDepth :: Int -> DeclarationLint
maxLambdaNestingDepth maxDepth =
  { name: RuleName "max-lambda-nesting-depth"
  , description:
      "Flags a lambda nested more than the configured depth inside other lambdas in the same declaration."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context decl -> case violations maxDepth decl of
      [] -> Passed
      vs -> Violation (Str.joinWith "; " vs)
  }

-- | Private. Used only by `maxLambdaNestingDepth`. Uses `onExpr`.
violations :: Int -> Declaration Void -> Array String
violations maxDepth decl = execState
  (rewriteDeclWithContextM (defaultVisitorWithContextM { onExpr = onExpr maxDepth }) 0 decl)
  []

-- | Private, depth 2. Used only by `violations`.
onExpr :: Int -> Int -> Expr Void -> State (Array String) (Int /\ Expr Void)
onExpr maxDepth depth expr = case expr of
  ExprLambda _ -> do
    let depth' = depth + 1
    when (depth' > maxDepth) $ modify_
      (_ <> [ "lambda nested " <> show depth' <> " deep, over the max of " <> show maxDepth ])
    pure (depth' /\ expr)
  _ -> pure (depth /\ expr)

-- Context: `maxLambdaNestingDepth` flags any lambda (`\x -> ...`) nested
-- more than `maxDepth` levels inside other lambdas within the same
-- declaration - same "hard to follow, consider extracting a helper"
-- reasoning as `PureScript.Lint.Rules.BranchNestingDepth`, kept as a separate rule
-- and a separate counter rather than merged into it: a lambda and a
-- branch point are different costs to a reader (closing over outer
-- arguments vs. following a decision), so conflating their depths would
-- hide which kind of nesting is actually the problem when one fires.
-- Same depth/traversal mechanics as `BranchNestingDepth` otherwise -
-- resets per top-level declaration, threads top-down via
-- `rewriteDeclWithContextM`, flags every offending lambda, not just the
-- first past the limit.
