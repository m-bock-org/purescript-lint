-- | The rule the README shows. It lives here so it is compiled, and is
-- | injected into the README from this file rather than written twice.
module Test.Lint.ReadmeExample
  ( arityRule
  , arityUnlessGenerated
  , generatedCode
  , maxFunctionArity
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String (contains) as Str
import PureScript.CST.Types (Declaration(..), Ident(..), Name(..))
import PureScript.CST.Types (Declaration) as CST
import Lint.Rule
  ( DeclarationLint
  , DeclarationRule
  , LintExemption
  , exclude
  , perDecl
  , violations
  )

maxFunctionArity :: DeclarationLint Int
maxFunctionArity =
  { name: "max-function-arity"
  , description: "Flags a function with more arguments than allowed."
  , examples: Just
      { config: 3
      , printConfig: \n -> Just ("max arity " <> show n)
      , good: [ "resize { width, height } img = img" ]
      , bad: [ "resize width height quality img = img" ]
      }
  , rule: \maxArity _context decl -> case decl of
      DeclValue { name: Name { name: Ident n }, binders }
        | Array.length binders > maxArity ->
            violations
              [ n <> " takes " <> show (Array.length binders) <> " args" ]
      _ -> violations []
  }

-- | The same rule, with and without an exemption.
arityRule :: DeclarationRule
arityRule = perDecl maxFunctionArity 3

arityUnlessGenerated :: DeclarationRule
arityUnlessGenerated = exclude [ generatedCode ] (perDecl maxFunctionArity 3)

generatedCode :: LintExemption (CST.Declaration Void)
generatedCode =
  { name: "generated code is not ours to shorten"
  , appliesTo: \ctx _ -> Str.contains (Pattern ".Generated.") ctx.moduleName
  }
