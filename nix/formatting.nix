{...}: {
  perSystem = {pkgs, ...}: {
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
  };
}
