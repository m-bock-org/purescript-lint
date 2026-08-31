<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/logo-dark.png">
  <img alt="purescript-lint" src="assets/logo-light.png" width="480">
</picture>

A lint engine for PureScript, written in PureScript. Rules are ordinary
values and a rule set is a program you write, so using it means writing
PureScript.

It reads a Spago workspace, parses each module with
[`language-cst-parser`](https://github.com/natefaubion/purescript-language-cst-parser),
and runs your rules over the CST. Some rules can fix what they find.

The engine ships with **no rules of its own**. Writing your own is the
expected path - see below.

**Status: early.** The API has had one consumer so far and will move.

## What a rule is

A rule is a record: a name, a description, an example of each side, and a
function from some piece of syntax to a verdict.

```purescript
maxFunctionArity :: Int -> DeclarationLint
maxFunctionArity maxArity =
  { name: "max-function-arity"
  , description: "Flags a function with more arguments than allowed."
  , goodExample: Just "resize { width, height } img = ..."
  , badExample: Just "resize width height quality img = ..."
  , rule: \_context decl -> case decl of
      DeclValue { name: Name { name: Ident n }, binders }
        | Array.length binders > maxArity ->
            violation
              (n <> " takes " <> show (Array.length binders) <> " args")
      _ -> violations []
  }
```

Configuration is an argument: `maxFunctionArity 4` is a rule, and
`maxFunctionArity 6` is a different one.

A verdict is built with `violation` for one finding, `violations` for
however many a rule found, or `fixed` with rewritten syntax. There is no
`passed`: a rule passes by finding nothing, and `violations []` says so,
so a rule that computes its findings needs no empty case. `withHint`
attaches a suggestion to all of them.

`fixed` makes a rule auto-fixable, and the bar for it is that no
judgment is left to a human - which most rules worth writing do not
clear.

You import the rules you want and put them in a list, so the compiler
tracks them: an unused import is a compile error, and a renamed rule
breaks the build at the call site.

## What a rule sees

Four levels, each a different unit of syntax:

| | sees | for |
|---|---|---|
| `perExpr` | one expression | nesting, escapes, branch shape |
| `perDecl` | one declaration | signatures, arity, naming |
| `perModule` | one module | imports, exports, line length |
| `perPackage` / `perWorkspace` | a survey of many modules | namespace and package-boundary rules |

## Defining a rule set

```purescript
import PureScript.Lint.RuleSet.Do (group, rule)
import PureScript.Lint.RuleSet.Do as Rules

myRules :: Array Rule
myRules = Rules.do
  group "Declarations" Rules.do
    rule $ perDecl (maxFunctionArity 4)
    rule $ perDecl myDeclarationRule

  group "Modules" Rules.do
    rule $ perModule myModuleRule
```

`group` is presentation only - it names a section in the output.

## Running it

```purescript
module Main where

import PureScript.Lint (runLinter)

main :: Effect Unit
main = launchAff_ do
  exitCode <- runLinter
    { rules: myRules
    , excludeRules: []
    , globalExclude: []
    }
  liftEffect (exit exitCode)
```

`excludeRules` names rules to skip outright; `globalExclude` names files
no rule should see. Run it from a Spago project directory.

## Licence

MIT.
