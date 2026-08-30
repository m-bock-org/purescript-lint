# purescript-lint

A lint engine for PureScript, written in PureScript. Rules are ordinary
values, so a rule set is a program you write rather than a config file you
fill in.

It reads a Spago workspace, parses each module with
[`language-cst-parser`](https://github.com/natefaubion/purescript-language-cst-parser),
and runs your rules over the CST. Some rules can fix what they find.

**Status: early.** The engine and the rules here are extracted from a
private codebase where they have been in daily use, but the API has had
exactly one consumer so far and will move.

## What a rule is

A rule is a record: a name, a description, an example of each side, and a
function from some piece of syntax to a verdict.

```purescript
unicodeForall :: DeclarationLint
unicodeForall =
  { name: RuleName "unicode-forall"
  , description: "Requires the unicode forall quantifier rather than the ASCII keyword."
  , goodExample: Just "identity :: ∀ a. a -> a"
  , badExample: Just "identity :: forall a. a -> a"
  , rule: \_context decl ->
      if hasAsciiForall decl then Fixed (toUnicode decl) else Passed
  }
```

A verdict is `Passed`, `Violation String`, or `Fixed` with the rewritten
syntax. Rules that return `Fixed` are auto-fixable; the bar for that is
that no judgment is left to a human.

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

The last two are *surveys*: a rule that needs to see the whole workspace
(how many modules share a namespace, say) gets a cheap structural map
rather than every module's CST at once.

## Defining a rule set

```purescript
import PureScript.Lint.RuleSet.Do as Rules

rules :: LintRuleSet
rules =
  { globalExclude: []
  , phases:
      [ { name: "all", rules: everything } ]
  }

everything :: Array Rule
everything = Rules.do
  group "How much fits on one line" Rules.do
    rule $ perModule (maxLineLength { code: 100, signature: 150 })
    rule $ perModule (maxDelimiterRun 2)

  group "How deep it goes" Rules.do
    rule $ perExpr (maxCallStackDepth 4)
    rule $ perDecl (maxLambdaNestingDepth 3)
```

`group` is presentation only - it names a section in the output. Phases
let one repo run a small set while iterating and the full set in CI.

Rules can be excluded per module, with a reason attached, so an exemption
records *why* rather than just switching something off.

## Rules that ship with it

Deliberately few, and all general. The interesting ones:

- **`maxCallStackDepth`** - how many of your own functions a call can pass
  through before reaching a leaf. The only rule here that charges for
  naming a thing, which makes it pull against every rule that rewards
  extraction. That tension is the point: when both exits close, the
  decomposition is usually wrong.
- **`maxDelimiterRun`** - how many brackets may open or close in a row.
  A blunt proxy for "this expression has too much going on".
- **`maxLineLength`** - separate budgets for code and for signatures,
  because a type that does not fit is usually saying something true.
- **`sameConstructorArm`** - a branch that matches `Right` and rebuilds
  `Right` around its result is a `map`.
- **`noStutteringName`** - `Foo.fooBar` repeats the qualifier.
- **`maxFunctionArity`**, **`maxDeclarationLines`**,
  **`maxLambdaNestingDepth`**, **`unicodeForall`**.

Every rule carries a trailing `## Context` block explaining why it exists
and what it is trading off. Those are the real documentation.

## Running it

```purescript
module Main where

import PureScript.Lint (runLinter)

main :: Effect Unit
main = launchAff_ do
  exitCode <- runLinter [] "all" rules
  liftEffect (exit exitCode)
```

It shells out to `spago ls packages --json` to find the workspace, so run
it from a repo root with a `spago.yaml`.

## Not yet

- No config file, by design - but also no way to use it without writing
  PureScript.
- No editor integration.
- Fixes are applied by the caller; the engine reports them.
- The survey API is the least settled part.

## Licence

MIT.
