{
  description = "A lint engine for PureScript source";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    al-dente.url = "git+ssh://git@github.com/m-bock/al-dente";
  };

  outputs = { self, nixpkgs, flake-utils, al-dente, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = al-dente.lib.${system};

        # A wrong hash makes nix report the right one.
        # What the editor runs, so it never reaches for a globally
        # installed compiler or the one under node_modules.
        toolchain = pkgs.symlinkJoin {
          name = "toolchain";
          paths = [ lib.defaults.purs lib.defaults.spago lib.defaults.purs-tidy ];
        };

        workspace = lib.mkWorkspace {
          src = ./.;
          name = "lint-purs";
          gitHashes = {
            codec-argonaut = "sha256-aio7wukXoRJKD+SKfgBiz9q6daVcReWmvGJNpQgZn44=";
            encode-decode = "sha256-urIWpgaeo0pimisUTzFzwNAIibAV31zGaHWbu2hEFuw=";
            patchdown = "sha256-H5vHK41/ceEpHt7dX34WLam13+TKooW86u4t3ZuXzJU=";
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

        checks.tests = pkgs.runCommand "lint-purs-tests" { } ''
          ${lib.mkRunner {
            name = "spec";
            mainModule = "Test.Main";
            output = workspace.testOutput;
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
