{
  description = "Relage project powered by flake-parts";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.treefmt-nix.flakeModule
        inputs.pre-commit-hooks.flakeModule
      ];

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem = {
        config,
        pkgs,
        ...
      }: {
        treefmt = {
          projectRootFile = "flake.nix";
          programs = {
            actionlint.enable = true;
            alejandra.enable = true;
            mdformat.enable = true;
            fourmolu = {
              enable = true;
              package = pkgs.haskell.packages.ghc912.fourmolu;
            };
          };
        };

        pre-commit = {
          settings = {
            hooks = {
              treefmt.enable = true;
            };
          };
        };

        packages = let
          hsPkgs = pkgs.haskell.packages.ghc912.extend (final: prev: {
            # Override opt-env-conf with version from Hackage (nixpkgs has 0.9, we need 0.13)
            opt-env-conf = final.callPackage ./nix/opt-env-conf.nix {};
            # dontCheck: CLI integration tests require 'cabal' which isn't available in the Nix sandbox.
            # Unit tests pass, but the integration test suite fails with "posix_spawnp: does not exist".
            # Run tests via 'cabal test' in the devShell instead.
            hs-spanshot = pkgs.haskell.lib.dontCheck (final.callCabal2nix "hs-spanshot" ./hs-spanshot {});
          });
        in {
          hs-spanshot = hsPkgs.hs-spanshot;
          default = hsPkgs.hs-spanshot;
        };

        apps.default = {
          type = "app";
          program = "${config.packages.hs-spanshot}/bin/spanshot";
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [config.pre-commit.devShell];
          buildInputs = with pkgs; [
            hello
            ollama
            cabal-install
            (pkgs.haskell-language-server.override {
              supportedGhcVersions = [
                "912"
              ];
            })
            haskell.compiler.ghc912
            ghcid
            hlint
            pkgs.haskell.packages.ghc912.fourmolu
            just
          ];
          shellHook = ''
            ${config.pre-commit.installationScript}
            echo "Welcome to the Relage development environment!"
            echo "Available packages: hello"
          '';
        };
      };

      flake = {
        # Add any flake-level configurations here
      };
    };
}
