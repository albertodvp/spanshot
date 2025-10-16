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

        packages = {
          hello = pkgs.hello;
          default = config.packages.hello;
        };

        devShells.default = pkgs.mkShell {
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
            haskellPackages.apply-refact
            fourmolu
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
