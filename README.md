# purescript-lint

A lint engine for PureScript, written in PureScript. Rules are ordinary
values, so a rule set is a program you write rather than a config file you
fill in - which also means there is no way to use it without writing
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
  { name: RuleName "max-function-arity"
  , description: "Flags a top-level function with more arguments than the configured maximum."
  , goodExample: Nothing
  , badExample: Nothing
  , rule: \_context decl -> case decl of
      DeclValue { name: Name { name: Ident n }, binders }
        | Array.length binders > maxArity ->
            Violation (n <> " takes " <> show (Array.length binders) <> ", over " <> show maxArity)
      _ -> Passed
  }
```

That is the whole rule - nothing is elided. Note that it is a *function*
returning a record: the configuration is just an argument, so
`maxFunctionArity 4` is a rule and there is no config file for it to be
configured from.

A verdict is `Passed`, `Violation String`, `ViolationWithHint`, or
`Fixed` with rewritten syntax. `Fixed` makes a rule auto-fixable, and the
bar for it is that no judgment is left to a human - which most rules
worth writing do not clear.

There is no plugin registry and no discovery. You import the rules you
want and put them in a list, which means an unused rule is a compile
error rather than a line of dead config.

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
import PureScript.Lint.RuleSet.Do as Rules

everything :: Array Rule
everything = Rules.do
  group "How much fits on one line" Rules.do
    rule $ perModule (maxLineLength { code: 100, signature: 150 })
    rule $ perModule (maxDelimiterRun 2)

  group "How deep it goes" Rules.do
    rule $ perExpr (maxCallStackDepth 4)
    rule $ perDecl (maxLambdaNestingDepth 3)
```

`group` is presentation only - it names a section in the output.

A rule set is an `Array Rule` and nothing more - no wrapper, no name. A
repo that wants more than one, a quick set while restructuring and the
full set in CI, writes two arrays, and a set that builds on another is
`quick <> rest`. That only works because there is nothing around it to
merge. Choosing between them at the command line is the caller's job,
which means an unrecognised name can be an error rather than a silently
empty run.

Rules can be excluded per module, with a reason attached, so an exemption
records *why* rather than just switching something off.

## Writing a rule

A rule is a value, so a new one is a module that exports a record. Here
is a whole rule, start to finish - it flags a top-level binding named
with a single letter:

```purescript
module MyRules.NoSingleLetterName (noSingleLetterName) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (length) as String
import PureScript.CST.Types (Declaration(..), Ident(..), Name(..))
import PureScript.Lint.Rule (DeclarationLint, LintResult(..), RuleName(..))

noSingleLetterName :: DeclarationLint
noSingleLetterName =
  { name: RuleName "no-single-letter-name"
  , description: "Flags a top-level binding whose name is a single character."
  , goodExample: Just "count = 3"
  , badExample: Just "n = 3"
  , rule: \_context decl -> case decl of
      DeclValue { name: Name { name: Ident ident } }
        | String.length ident == 1 ->
            Violation ("top-level `" <> ident <> "` needs a name, not a letter")
      _ -> Passed
  }
```

That is all of it. There is no registration step and no manifest - you
put it in your rule set and it runs:

```purescript
group "Naming" Rules.do
  rule $ perDecl noSingleLetterName
```

Three things are worth knowing before you write your own.

**Pick the smallest scope that can see the answer.** The type you choose
- `DeclarationLint`, `ExprLint`, `ModuleLint` - decides what your
function is handed and how often it runs. A rule that only needs one
declaration should not ask for the module.

**Return `Fixed` only when no judgment is left.** `Passed`, `Violation
String` and `ViolationWithHint` all leave the decision with a person.
`Fixed` hands back rewritten syntax and the caller applies it, so the bar
is that there is exactly one right answer - `unicodeForall` qualifies,
`maxFunctionArity` never could.

**Write the `## Context` block.** Every rule in the rule set ends with a
trailing comment saying why it exists and what it trades away. A rule
that cannot justify itself in a paragraph usually should not be a rule -
and the paragraph is what a reader hits when the rule fires on them.

## Running it

```purescript
module Main where

import PureScript.Lint (runLinter)

main :: Effect Unit
main = launchAff_ do
  exitCode <- runLinter [] [] everything
  liftEffect (exit exitCode)
```

It shells out to `spago ls packages --json` to find the workspace, so run
it from a repo root with a `spago.yaml`.

## Not yet

- No editor integration.
- Fixes are applied by the caller; the engine reports them.
- The survey API is the least settled part.

## Licence

MIT.
