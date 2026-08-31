-- | The rule the README shows. It lives here so it is compiled, and is
-- | injected into the README from this file rather than written twice.
module Test.PureScript.Lint.ReadmeExample (maxFunctionArity) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import PureScript.CST.Types (Declaration(..), Ident(..), Name(..))
import PureScript.Lint.Rule (DeclarationLint, violations)

maxFunctionArity :: Int -> DeclarationLint
maxFunctionArity maxArity =
  { name: "max-function-arity"
  , description: "Flags a function with more arguments than allowed."
  , goodExample: Just "resize { width, height } img = ..."
  , badExample: Just "resize width height quality img = ..."
  , rule: \_context decl -> case decl of
      DeclValue { name: Name { name: Ident n }, binders }
        | Array.length binders > maxArity ->
            violations [ n <> " takes " <> show (Array.length binders) <> " args" ]
      _ -> violations []
  }
