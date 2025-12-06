{...}: {
  perSystem = {
    pkgs,
    lib,
    config,
    ...
  }: {
    devShells.default = pkgs.mkShell {
      inputsFrom = [config.pre-commit.devShell];
      buildInputs = with pkgs; [
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
