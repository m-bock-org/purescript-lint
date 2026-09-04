<p>
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="assets/logo-dark.png">
    <img alt="purescript-lint" src="assets/logo-light.png" width="480">
  </picture>
</p>

[![CI](https://github.com/m-bock/purescript-lint/actions/workflows/ci.yml/badge.svg)](https://github.com/m-bock/purescript-lint/actions/workflows/ci.yml)

A lint engine for PureScript, written in PureScript. A rule set is a
program you write, so using it means writing PureScript.

It reads a Spago workspace, parses each module with
[`language-cst-parser`](https://github.com/natefaubion/purescript-language-cst-parser),
and runs your rules over the CST. Some rules can fix what they find.

The engine ships with **no rules of its own**. Writing your own is the
expected path.

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
function from a setting and some piece of syntax to a verdict.

<!-- PD_START:purs
filePath: test/Test/Lint/ReadmeExample.purs
pick:
  - tag: value_and_signature
    name: maxFunctionArity
-->

```purescript
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
```

<!-- PD_END -->

A rule is a family rather than one check. Its setting arrives where it
joins a rule set, so `perDecl maxFunctionArity 4` and
`perDecl maxFunctionArity 6` come from one definition, and every setting
a project runs at is visible in the one file that assembles them. A rule
with nothing to configure uses `perDecl_`.

Examples are read against a setting of their own - two nested lambdas are
fine at a depth of two and a violation at one - so they carry it, and
`printConfig` is how the rule words it for a report. `Just <<< show` where
the value speaks for itself, `\_ -> Nothing` where there is nothing to
configure.

<!-- PD_START:purs
filePath: src/Lint/Internal/Rule.purs
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

A rule is written at one of these levels:

| | sees |
|---|---|
| `perExpr` | one expression |
| `perDecl` | one declaration |
| `perModule` | one module |
| `perPackage` / `perWorkspace` | a survey of many modules |

Each takes the rule and its setting. The `_` variants - `perDecl_` and
the rest - are for rules with nothing to configure.

## Defining a rule set

```purescript
import Lint.RuleSet.Do (group, rule)
import Lint.RuleSet.Do as Rules

myRules :: Array Rule
myRules = Rules.do
  group "Declarations" Rules.do
    rule $ perDecl maxFunctionArity 4
    rule $ perDecl_ myDeclarationRule

  group "Modules" Rules.do
    rule $ perModule_ myModuleRule
```

## Exemptions

Switching a rule off for something is done by naming the reason, so the
exemption reads as documentation where it is applied. Here the arity rule
skips generated modules:

<!-- PD_START:purs
filePath: test/Test/Lint/ReadmeExample.purs
pick:
  - tag: value_and_signature
    name: arityUnlessGenerated
  - tag: value_and_signature
    name: generatedCode
-->

```purescript
arityUnlessGenerated :: DeclarationRule
arityUnlessGenerated = exclude [ generatedCode ] (perDecl maxFunctionArity 3)

generatedCode :: LintExemption (CST.Declaration Void)
generatedCode =
  { name: "generated code is not ours to shorten"
  , appliesTo: \ctx _ -> Str.contains (Pattern ".Generated.") ctx.moduleName
  }
```

<!-- PD_END -->

`exclude` and `runLinterWith { skipModules }` decide before a check runs -
one value for one rule, or every rule for a whole module. `ignoreSubjects`
decides after: a survey rule is handed the workspace in one go, so its
findings can only be dropped once made, by what each one is about.

## Running it

<!-- PD_START:purs
filePath: src/Lint.purs
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
type LintOptions =
  { skipModules :: Array ModuleExemption
  , fix :: Maybe FixConfig
  -- | Which exemptions this run honours. `All` for a person's run;
  -- | `ByDesignOnly` for a fixer, which is the one caller that should
  -- | see the pending backlog rather than have it suppressed.
  , standing :: Exemptions.Standing
  }
```

<!-- PD_END -->

```purescript
module Main where

import Node.Process (exit')
import Lint (runLinter)

main :: Effect Unit
main = launchAff_ do
  clean <- runLinter myRules
  liftEffect (exit' (if clean then 0 else 1))
```

Run it from a Spago project directory.

## What it prints

Grouped by the rule that fired: what the rule wants, then every place it
was not met. A rule explains itself once, however many things it found.

```
● Declarations » max-function-arity
    Flags a function with more arguments than allowed.
      with  max arity 3
      good  resize { width, height } img = img
      bad   resize width height quality img = img

  ◦ MyApp.Image
      resize takes 4 args

  ◦ MyApp.Layout
      place takes 5 args

Summary: 2 findings in 2 of 9 modules
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
