{
  description = "Relage project powered by flake-parts";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem =
        {
          config,
          pkgs,
          ...
        }:
        {
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
