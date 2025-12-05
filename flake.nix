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
            hs-spanshot = final.callCabal2nix "hs-spanshot" ./hs-spanshot {};
          });
          # Add preCheck hook to make the spanshot binary available during tests.
          # The CLI integration tests need to find the executable, which is built
          # at dist/build/spanshot/spanshot during the Nix build.
          hs-spanshot-tested = hsPkgs.hs-spanshot.overrideAttrs (old: {
            preCheck =
              (old.preCheck or "")
              + ''
                export SPANSHOT_BIN="$PWD/dist/build/spanshot/spanshot"
              '';
          });
          # Coverage build - runs tests with HPC instrumentation, outputs to $out/share/hpc/
          hs-spanshot-coverage = pkgs.haskell.lib.doCoverage hs-spanshot-tested;
          # Codecov JSON report - converts HPC coverage data to Codecov format
          hs-spanshot-codecov-report =
            pkgs.runCommand "hs-spanshot-codecov-report" {
              nativeBuildInputs = [pkgs.haskellPackages.hpc-codecov pkgs.jq];
              coverage = hs-spanshot-coverage;
            } ''
              MIX_DIR=$(find $coverage/lib -type d -path "*/vanilla/mix" | head -1)
              TIX_FILE=$(find $coverage/share/hpc -name "*.tix" | head -1)

              mkdir -p $out

              # Generate report with nix store paths
              hpc-codecov \
                --mix="$MIX_DIR" \
                --src=${./hs-spanshot} \
                --exclude=Main,Paths_hs_spanshot,WindowManagementSpec,SerializationProperties,Fixtures,DetectionSpec,CollectionSpec,CaptureTypesSpec,CaptureStreamSpec \
                --out=$out/codecov-raw.json \
                "$TIX_FILE"

              # Rewrite nix store paths to relative repository paths
              # /nix/store/xxx-hs-spanshot/src/File.hs -> hs-spanshot/src/File.hs
              jq '.coverage | to_entries | map({key: (.key | sub("/nix/store/[^/]+-hs-spanshot/"; "hs-spanshot/")), value: .value}) | from_entries | {coverage: .}' \
                $out/codecov-raw.json > $out/codecov.json
              rm $out/codecov-raw.json
            '';
        in {
          hs-spanshot = hs-spanshot-tested;
          default = hs-spanshot-tested;
          inherit hs-spanshot-coverage hs-spanshot-codecov-report;
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
    };
}
