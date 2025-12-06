{
  description = "Spanshot - log capture and error detection tool";

  nixConfig = {
    extra-substituters = ["https://spanshot.cachix.org"];
    extra-trusted-public-keys = ["spanshot.cachix.org-1:3TOagW6wPJNA18zei8rkUWT9eZn13fj86qJ2JN2rUQk="];
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    agenix-shell = {
      url = "github:aciceri/agenix-shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
        ./nix/secrets
        ./nix/packages.nix
        ./nix/apps.nix
        ./nix/devshell.nix
        ./nix/formatting.nix
      ];

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
    };
}
