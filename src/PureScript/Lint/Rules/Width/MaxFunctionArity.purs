module PureScript.Lint.Rules.Width.MaxFunctionArity (maxFunctionArity) where

import Prelude

import Data.Array (length) as Array
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import PureScript.CST.Types (Declaration(..), Ident(..), Name(..))
import PureScript.Lint.Rule (DeclarationLint, LintResult(..), RuleName(..))

maxFunctionArity :: Int -> DeclarationLint
maxFunctionArity maxArity =
  { name: RuleName "max-function-arity"
  , description:
      "Flags a top-level function definition with more than the configured number of arguments."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context decl -> case decl of
      DeclValue { name: Name { name: Ident n }, binders }
        | Array.length binders > maxArity -> Violation $ fold
            [ n
            , " takes "
            , show (Array.length binders)
            , " arguments, over the max of "
            , show maxArity
            , " - group related ones into a record, or a tuple where they have no good names"
            ]
      _ -> Passed
  }

-- Context: `maxFunctionArity` flags a top-level function definition whose
-- own binder list is longer than `maxArity` - a proxy for "this function
-- is asking a caller to track too many positional arguments at once,
-- consider grouping related ones into a record." A record is the usual
-- answer, because the parameters that pile up are the ones that already
-- travel together and each has a name worth keeping. A tuple is the
-- other option and occasionally the better one - for a pair with no
-- names worth inventing, `Tuple`/`/\` says "these two move as one"
-- without making up field labels for them. Declaration-level only,
-- not a full traversal: checks `DeclValue`'s own `binders` directly,
-- unlike the nesting-depth rules, which need
-- `PureScript.CST.Traversal.rewriteDeclWithContextM` to see the whole
-- expression tree. Doesn't look inside the function body for further
-- lambdas of their own arity - a narrower scope than the nesting rules,
-- deliberately: a curried inner lambda's own arity is a separate concern
-- from how many arguments the function itself is called with.
