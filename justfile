# Deliberately NOT putting node_modules/.bin on PATH. package.json still
# carries purs/spago/purs-tidy because CI installs them with npm, but a
# recipe run inside `nix develop` must use the compiler the flake pins -
# and node_modules/.bin first on PATH silently wins over it. The two
# agreed at 0.15.16 by luck; ^0.15.15 drifts on the next npm install.
set shell := ["bash", "-c"]

build:
    spago build

# The gate's build: warnings are errors. purs does not re-report a
# warning for a module it did not recompile, so an incremental strict
# build can print zero while warnings genuinely exist. Dropping our own
# modules' output makes the count real without rebuilding every
# dependency, whose warnings are not ours to fix anyway.
strict:
    #!/usr/bin/env bash
    set -euo pipefail
    grep -rhoE '^module [A-Za-z0-9_.]+' src test 2>/dev/null \
      | awk '{print $2}' \
      | while read -r m; do rm -rf "output/$m"; done
    spago build --strict

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

check: strict test docs-check

# Restore output/ from the Nix build rather than compiling it here. A
# copy, not symlinks: purs writes into output/<Module>/ in place, and a
# read-only store symlink dies on the first local edit.
output:
    #!/usr/bin/env bash
    set -euo pipefail
    built="$(nix build .#default --no-link --print-out-paths)"
    rm -rf output
    cp -aL "$built" output
    chmod -R u+w output
    echo "output/ restored from $built"

