<p>
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="assets/logo-dark.png">
    <img alt="purescript-lint" src="assets/logo-light.png" width="480">
  </picture>
</p>

A lint engine for PureScript, written in PureScript. A rule set is a
program you write, so using it means writing PureScript.

It reads a Spago workspace, parses each module with
[`language-cst-parser`](https://github.com/natefaubion/purescript-language-cst-parser),
and runs your rules over the CST. Some rules can fix what they find.

The engine ships with **no rules of its own**. Writing your own is the
expected path - see below.

**Status: early.** The API has had one consumer so far and will move.

- [Installation](#installation)
- [What a rule is](#what-a-rule-is)
- [What a rule sees](#what-a-rule-sees)
- [Defining a rule set](#defining-a-rule-set)
- [Exemptions](#exemptions)
- [Running it](#running-it)
- [What it prints](#what-it-prints)
- [Working on it](#working-on-it)
- [Contributing](#contributing)
- [Licence](#licence)

## Installation

Add these to `extraPackages`, and `lint-purs` to your `dependencies`.

<details>
<summary>The <code>extraPackages</code> entries</summary>

```yaml
workspace:
  extraPackages:
    lint-purs:
      git: https://github.com/m-bock/purescript-lint.git
      ref: <commit>
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
      ref: <commit>
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

</details>

## What a rule is

A rule is a record: a name, a description, an example of each side, and a
function from some piece of syntax to a verdict.

<!-- PD_START:purs
filePath: test/Test/PureScript/Lint/ReadmeExample.purs
pick:
  - tag: value_and_signature
    name: maxFunctionArity
-->

```purescript
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
```

<!-- PD_END -->

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

| | sees |
|---|---|
| `perExpr` | one expression |
| `perDecl` | one declaration |
| `perModule` | one module |
| `perPackage` / `perWorkspace` | a survey of many modules |

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

## Exemptions

Switching a rule off for something is done by naming the reason, so the
exemption reads as documentation where it is applied. Here the arity rule
skips generated modules:

<!-- PD_START:purs
filePath: test/Test/PureScript/Lint/ReadmeExample.purs
pick:
  - tag: value_and_signature
    name: arityUnlessGenerated
  - tag: value_and_signature
    name: generatedCode
-->

```purescript
arityUnlessGenerated :: DeclarationRule
arityUnlessGenerated = exclude [ generatedCode ] (perDecl (maxFunctionArity 4))

generatedCode :: LintExemption (CST.Declaration Void)
generatedCode =
  { name: "generated code is not ours to shorten"
  , appliesTo: \ctx _ -> contains (Pattern ".Generated.") ctx.moduleName
  }
```

<!-- PD_END -->

There are three scopes. `exclude` skips one value for one rule, as above.
`excludeSubjects` does the same for a survey rule's findings. And
`runLinterWith { skipModules }` skips a module for every rule at once,
which is the cheaper one - it is decided before any rule runs.

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

Grouped by the rule that fired: what the rule wants, then every place it
was not met. A rule explains itself once, however many things it found.

```
Declarations / max-function-arity
  Flags a function with more arguments than allowed.
    good  resize { width, height } img = ...
    bad   resize width height img = ...

  MyApp.Image
    resize takes 3 args

  MyApp.Layout
    place takes 5 args

2 findings in 2 of 9 modules
```

## Working on it

The toolchain is pinned in `package.json`, so install it first:

```
npm install
```

Then, with `node_modules/.bin` on `PATH` (which the `justfile` does for
you):

```
just build
just test
```

## Contributing

Contributions are welcome! Please [open an issue](https://github.com/m-bock/purescript-lint/issues/new) to report bugs, suggest improvements, or propose new rules to be added.

If this project was useful to you, a virtual coffee is appreciated.

<a href="https://ko-fi.com/mbock">
  <img src="https://ko-fi.com/img/githubbutton_sm.svg" alt="Buy Me a Coffee at ko-fi.com" />
</a>

## Licence

MIT.
