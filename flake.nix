{
  description = "A lint engine for PureScript source";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    al-dente.url = "git+ssh://git@github.com/m-bock-org/al-dente";

    # Generated from spago.lock by `just inputs-sync` - every git
    # dependency needs one, because evaluation does not fetch.
    # al-dente:git-inputs:begin
    "codec-argonaut" = { url = "github:garyb/purescript-codec-argonaut/dc287c83b79f86f8d9a0045dcb740f0cf9d23ccd"; flake = false; };
    "encode-decode" = { url = "github:m-bock/purescript-encode-decode/07a361b0e42314ee6521b8ccc774eca117be57d0"; flake = false; };
    "patchdown" = { url = "github:m-bock/purescript-patchdown/7012c7a839d7c2f8e9d1a8acf9a56d9435eaccc4"; flake = false; };
    # al-dente:git-inputs:end
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, al-dente, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = al-dente.lib.${system};

        # A wrong hash makes nix report the right one.
        # What the editor runs, so it never reaches for a globally
        # installed compiler or the one under node_modules.
        toolchain = lib.toolchain;

        workspace = lib.mkWorkspace {
          src = ./.;
          name = "lint-purs";
          gitPaths = {
            # al-dente:git-paths:begin
            "codec-argonaut" = inputs."codec-argonaut";
            "encode-decode" = inputs."encode-decode";
            "patchdown" = inputs."patchdown";
            # al-dente:git-paths:end
          };
        };
      in
      {
        packages.default = workspace.output;

        # The npm side of this repo, built by Nix rather than by a `npm ci`
        # you have to remember. Only patchdown needs it - its FFI imports
        # js-yaml at runtime - but "only" is how a second way to build
        # gets in. --ignore-scripts because the purescript/spago/purs-tidy
        # entries in package.json fetch binaries on install: CI still uses
        # them, nothing here does.
        packages.nodeModules = pkgs.buildNpmPackage {
          pname = "lint-purs-node-modules";
          version = "0.0.1";
          src = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [ ./package.json ./package-lock.json ];
          };
          npmDepsHash = "sha256-QZf4yf78uVWml4qWOb9a4MfYpDdEsY6lv6yeFNQru98=";
          npmFlags = [ "--ignore-scripts" ];
          dontNpmBuild = true;
          installPhase = ''
            runHook preInstall
            cp -r node_modules $out
            runHook postInstall
          '';
        };

        # What the editor runs, so it never reaches for a globally
        # installed compiler or the one under node_modules.
        packages.toolchain = toolchain;
        # The compiled test closure - every dependency plus the local
        # packages built with their tests. `just output` copies this so a
        # dev shell never recompiles what al-dente already built once per
        # machine, which is the whole point of building with al-dente.
        packages.testOutput = workspace.testOutput;

        # Makes `output/` match the built closure, and does nothing when
        # it already does - so a build can depend on it unconditionally.
        #
        # The hand-rolled version this replaces guarded on "output/ is
        # missing", which al-dente's own note calls out as the wrong
        # guard: bumping a dependency leaves a directory that exists and
        # is stale, and spago answers that by compiling every dependency
        # from source. That is what turned a one-line pin change into a
        # 541-module rebuild, and it is the granularity this workspace
        # is built to have.
        packages.restoreOutput = lib.mkRestore { output = workspace.testOutput; };

        # Writes this flake's git inputs from spago.lock, so the
        # revision is recorded once rather than in two files.
        packages.syncFlakeInputs = lib.syncInputs;

        # The README is generated from the sources it documents, so it
        # can go stale silently. This regenerates it and diffs against
        # what is committed - the same thing `just docs-check` does,
        # against the tree a pull request actually carries.
        checks.docs = pkgs.runCommand "docs" { } ''
          cp -a ${self} src
          chmod -R u+w src
          cd src
          cp README.md before.md
          PATCHDOWN_FILE_PATH="./README.md" \
            ${lib.mkRunner {
              name = "patchdown";
              mainModule = "Patchdown";
              output = workspace.output;
              nodeModules = self.packages.${system}.nodeModules;
            }}/bin/patchdown >/dev/null
          diff -u before.md README.md
          touch $out
        '';

        # Run from a copy of the tree, not from the store alone. This
        # suite is a linter's, so it reads the workspace it is pointed
        # at: it shells out to `spago ls packages --json` and opens its
        # own fixtures by relative path. Given only the compiled output
        # it fails on a missing spago and a missing file - which it did,
        # on main, for as long as CI ran `just check` inside a dev shell
        # that happened to supply both.
        checks.tests = pkgs.runCommand "lint-purs-tests"
          {
            nativeBuildInputs = [
              lib.defaults.spago
              lib.defaults.purs
              pkgs.nodejs
              pkgs.git
              pkgs.jq
            ];
          } ''
          cp -a ${self} src
          chmod -R u+w src
          cd src
          cp -a ${workspace.dotSpago}/. .spago
          chmod -R u+w .spago
          export HOME="$TMPDIR/home"
          mkdir -p "$HOME/.cache/spago-nodejs"
          touch "$HOME/.cache/spago-nodejs/fresh-registry-canary.txt"
          ${lib.mkRunner {
            name = "spec";
            mainModule = "Test.Main";
            output = workspace.testOutput;
            nodeModules = self.packages.${system}.nodeModules;
          }}/bin/spec
          touch $out
        '';

        devShells.default = pkgs.mkShell {
          name = "lint-purs";

          # A marker that you are inside the dev shell. `nix develop` used
          # to do this itself and stopped, and the difference matters here:
          # outside it, `purs` is whatever is installed globally.
          shellHook = ''
            case $- in *i*) export PS1="(lint) $PS1" ;; esac

            # Point the editor at this exact toolchain. Done here rather
            # than by a command you have to remember, and refreshed on
            # every entry, so it cannot go stale against the flake - the
            # editor and the build stay the same compiler by
            # construction. The .vscode wrappers read this symlink and
            # then need no nix at all.
            ln -sfn ${toolchain} .vscode/.toolchain

            # patchdown's FFI imports js-yaml at runtime, so the docs
            # step needs node_modules. Linked from the store rather than
            # left to `npm ci`, so there is one way to get a working tree.
            if [ ! -e node_modules ]; then
              ln -s ${self.packages.${system}.nodeModules} node_modules
            fi
          '';

          packages = [
            lib.defaults.purs
            lib.defaults.spago
            lib.defaults.purs-tidy
            lib.defaults.nodejs
            pkgs.just
            pkgs.git
          ];
        };
      });
}
