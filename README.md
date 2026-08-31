<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/logo-dark.png">
  <img alt="purescript-lint" src="assets/logo-light.png" width="480">
</picture>

<br>

A lint engine for PureScript, written in PureScript. A rule set is a
program you write, so using it means writing PureScript.

It reads a Spago workspace, parses each module with
[`language-cst-parser`](https://github.com/natefaubion/purescript-language-cst-parser),
and runs your rules over the CST. Some rules can fix what they find.

The engine ships with **no rules of its own**. Writing your own is the
expected path - see below.

**Status: early.** The API has had one consumer so far and will move.

## Installation

Not on the registry yet, so add it to `extraPackages` by commit. Its own
dependency `encode-decode` is not on the registry either, so that one
needs an entry too:

```yaml
workspace:
  extraPackages:
    lint-purs:
      git: https://github.com/m-bock/purescript-lint.git
      ref: d2740bc5b55c88833101e8ac3ed700989e8407d3
      dependencies:
        - aff
        - arrays
        - console
        - effect
        - either
        - encode-decode
        - foldable-traversable
        - foreign-object
        - language-cst-parser
        - maybe
        - node-buffer
        - node-child-process
        - node-fs
        - node-glob-basic
        - node-path
        - ordered-collections
        - prelude
        - strings
        - transformers
        - tuples
    encode-decode:
      git: https://github.com/m-bock/purescript-encode-decode.git
      ref: 07a361b0e42314ee6521b8ccc774eca117be57d0
      dependencies:
        - argonaut-core
        - argonaut-codecs
        - arrays
        - contravariant
        - either
        - foldable-traversable
        - foreign-object
        - heterogeneous
        - maybe
        - ordered-collections
        - prelude
        - record
        - transformers
        - tuples
        - typelevel-prelude
```

Then add `lint-purs` to your package's `dependencies`.

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

<!-- PD_START:purs
filePath: src/PureScript/Lint/Internal/Rule.purs
pick:
  - tag: signature
    name: violations
  - tag: signature
    name: fixed
  - tag: signature
    name: withHint
-->

```purescript
violations :: ∀ a. Array String -> LintResult a
fixed :: ∀ a. a -> LintResult a
withHint :: ∀ a. String -> LintResult a -> LintResult a
```

<!-- PD_END -->

A verdict is `violations` with whatever a rule found, or `fixed` with
rewritten syntax. A rule passes by finding nothing, so `violations []`
is how it says so, and a rule that computes its findings hands them
over as they come. `withHint` attaches a suggestion to all of them.

`fixed` makes a rule auto-fixable, and the bar for it is that no
judgment is left to a human - which most rules worth writing do not
clear.

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

<!-- PD_START:purs
filePath: src/PureScript/Lint.purs
pick:
  - tag: signature
    name: runLinter
  - tag: signature
    name: runLinterWith
  - tag: type
    name: LintOptions
-->

```purescript
runLinter :: Array Rule -> Aff Boolean
runLinterWith :: LintOptions -> Array Rule -> Aff Boolean
type LintOptions = { skipModules :: Array ModuleExemption }
```

<!-- PD_END -->

```purescript
module Main where

import PureScript.Lint (runLinter)

main :: Effect Unit
main = launchAff_ do
  clean <- runLinter myRules
  liftEffect (exit (if clean then 0 else 1))
```

Run it from a Spago project directory.

## What it prints

Each finding names the module, says what is wrong, and which rule says
so. Each rule that fired then explains itself once, however many things
it found.

```
Linter: 2 rule(s)
  Data.Json.Decode: 42 decls, over 40  [max-decls]
  Data.Json.Decode.Sum: jErr is 4 chars (hint: spell it out)  [min-name]
  Data.Json.Encode: 42 decls, over 40  [max-decls]
  Test.Data.Json.RecordSpec: spec is 4 chars (hint: spell it out)  [min-name]
  Test.Data.Json.SumSpec: spec is 4 chars (hint: spell it out)  [min-name]
  Test.Data.JsonSpec: spec is 4 chars (hint: spell it out)  [min-name]
  Test.Main: main is 4 chars (hint: spell it out)  [min-name]

  max-decls
    Flags a module with more top-level declarations than allowed.
  min-name
    Flags a top-level name too short to say what it is.
    good: decodeRecord = ...
    bad:  dec = ...

Linter: 7 violation(s)
```

## Contributing

Contributions are welcome! Please [open an issue](https://github.com/m-bock/purescript-lint/issues/new) to report bugs, suggest improvements, or propose new rules to be added.

If this project was useful to you, a virtual coffee is appreciated.

<a href="https://ko-fi.com/mbock">
  <img src="https://ko-fi.com/img/githubbutton_sm.svg" alt="Buy Me a Coffee at ko-fi.com" />
</a>

## Licence

MIT.
