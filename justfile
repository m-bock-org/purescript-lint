# Deliberately NOT putting node_modules/.bin on PATH. package.json still
# carries purs/spago/purs-tidy because CI installs them with npm, but a
# recipe run inside `nix develop` must use the compiler the flake pins -
# and node_modules/.bin first on PATH silently wins over it. The two
# agreed at 0.15.16 by luck; ^0.15.15 drifts on the next npm install.
set shell := ["bash", "-c"]

# Restores output/ first if there is none - a fresh clone then compiles
# nothing, because al-dente already built every dependency. Only when it
# is missing: once you have edited anything, the store copy is behind
# your working tree and replacing output/ would throw away exactly the
# incremental state that makes a rebuild fast.
build:
    #!/usr/bin/env bash
    set -euo pipefail
    [ -d output ] || just output
    spago build

format:
    purs-tidy format-in-place 'src/**/*.purs'

test:
    spago test

docs:
    PATCHDOWN_FILE_PATH="./README.md" spago run -m Patchdown

# fails if the README's injected blocks are out of date with the source
docs-check:
    #!/usr/bin/env bash
    set -euo pipefail
    before=$(mktemp)
    cp README.md "$before"
    PATCHDOWN_FILE_PATH="./README.md" spago run -m Patchdown >/dev/null
    diff -u "$before" README.md

check: test docs-check

# Restore output/ from the Nix build rather than compiling it here. A
# copy, not symlinks: purs writes into output/<Module>/ in place, and a
# read-only store symlink dies on the first local edit.
output:
    nix run .#restoreOutput

# The style, run as a binary rather than as a dependency.
#
# A dependency is not available to every repository - the regulator
# depends on several of them, so the arrow only goes one way - and the
# binary needs none: it reads the workspace it is run in, and
# `lint-exemptions.json` beside this file is where a departure goes.
#
# `--fix <command>` names a program that proposes fixes for findings the
# style has guidance for. The linter judges what comes back.
# Pinned by rev, and it has to be. Unpinned, this fetched whatever the
# linter's main happened to be when CI ran - so this repository could go
# red without anyone touching it, and the rule set moved three times in
# one afternoon. A gate that moves under you is not a gate.
lint *ARGS:
    nix run 'git+ssh://git@github.com/m-bock-org/purescript-lint-regulator?rev=17abc2b05e8f89de935404510a447119c478e649#lint-public' -- {{ARGS}}
