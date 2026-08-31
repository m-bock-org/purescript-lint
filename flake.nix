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

        # What the editor runs, so it never reaches for a globally
        # installed compiler or the one under node_modules.
        packages.toolchain = pkgs.symlinkJoin {
          name = "toolchain";
          paths = [ lib.defaults.purs lib.defaults.spago lib.defaults.purs-tidy ];
        };

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
