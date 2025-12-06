{...}: {
  perSystem = {pkgs, ...}: {
    packages = let
      hsPkgs = pkgs.haskell.packages.ghc912.extend (final: prev: {
        # Override opt-env-conf with version from Hackage (nixpkgs has 0.9, we need 0.13)
        opt-env-conf = final.callPackage ./opt-env-conf.nix {};
        hs-spanshot = final.callCabal2nix "hs-spanshot" ../hs-spanshot {};
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
            --src=${../hs-spanshot} \
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
  };
}
