{...}: {
  perSystem = {
    pkgs,
    lib,
    config,
    ...
  }: {
    devShells.default = let
      hsPkgs = pkgs.haskell.packages.ghc912.extend (final: prev: {
        opt-env-conf = final.callPackage ./opt-env-conf.nix {};
        hs-spanshot = final.callCabal2nix "hs-spanshot" ../hs-spanshot {};
      });
    in
      # Use shellFor to automatically get all dependencies from cabal file
      hsPkgs.shellFor {
        packages = p: [p.hs-spanshot];
        nativeBuildInputs = with pkgs; [
          ollama
          cabal-install
          (pkgs.haskell-language-server.override {
            supportedGhcVersions = ["912"];
          })
          ghcid
          hlint
          hsPkgs.fourmolu
          ragenix
          age
          just
        ];
        shellHook = ''
          ${config.pre-commit.installationScript}
          echo "Welcome to the Spanshot development environment!"
          source ${lib.getExe config.agenix-shell.installationScript}
        '';
      };
  };
}
