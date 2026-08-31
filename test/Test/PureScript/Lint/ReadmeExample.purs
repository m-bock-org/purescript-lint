-- | The rule the README shows. It lives here so it is compiled, and is
-- | injected into the README from this file rather than written twice.
module Test.PureScript.Lint.ReadmeExample
  ( arityRule
  , arityUnlessGenerated
  , generatedCode
  , maxFunctionArity
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import PureScript.CST.Types (Declaration(..), Ident(..), Name(..))
import PureScript.CST.Types (Declaration) as CST
import PureScript.Lint.Rule
  ( DeclarationLint
  , DeclarationRule
  , LintExemption
  , exclude
  , perDecl
  , violations
  )

maxFunctionArity :: Int -> DeclarationLint
maxFunctionArity maxArity =
  { name: "max-function-arity"
  , description: "Flags a function with more arguments than allowed."
  , goodExample: Just "resize { width, height } img = ..."
  , badExample: Just "resize width height img = ..."
  , rule: \_context decl -> case decl of
      DeclValue { name: Name { name: Ident n }, binders }
        | Array.length binders > maxArity ->
            violations
              [ n <> " takes " <> show (Array.length binders) <> " args" ]
      _ -> violations []
  }

-- | The same rule, with and without an exemption.
arityRule :: DeclarationRule
arityRule = perDecl (maxFunctionArity 4)

arityUnlessGenerated :: DeclarationRule
arityUnlessGenerated = exclude [ generatedCode ] (perDecl (maxFunctionArity 4))

generatedCode :: LintExemption (CST.Declaration Void)
generatedCode =
  { name: "generated code is not ours to shorten"
  , appliesTo: \ctx _ -> contains (Pattern ".Generated.") ctx.moduleName
  }
