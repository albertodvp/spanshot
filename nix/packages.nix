{...}: {
  perSystem = {pkgs, ...}: {
    packages = let
      hsPkgs = pkgs.haskell.packages.ghc912.extend (final: prev: {
        # Override autodocodec with version from Hackage (required by opt-env-conf and hs-spanshot)
        autodocodec = final.callPackage ./autodocodec.nix {};
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
      # Coverage build - runs only unit tests (not integration-cli) with HPC instrumentation
      # The integration-cli tests don't import the library, so they don't contribute
      # to library coverage and would overwrite the unit test coverage data.
      hs-spanshot-coverage = (pkgs.haskell.lib.doCoverage hs-spanshot-tested).overrideAttrs (old: {
        # Only run unit tests for coverage, skip integration-cli
        checkPhase = ''
          runHook preCheck
          ./Setup test hs-spanshot-test
          runHook postCheck
        '';
      });
      # Wrap package to include 'spns' alias alongside 'spanshot'
      hs-spanshot-with-alias = pkgs.symlinkJoin {
        name = "spanshot";
        paths = [hs-spanshot-tested];
        postBuild = ''
          ln -s $out/bin/spanshot $out/bin/spns
        '';
      };
      # Codecov JSON report - converts HPC coverage data to Codecov format
      hs-spanshot-codecov-report =
        pkgs.runCommand "hs-spanshot-codecov-report" {
          nativeBuildInputs = [pkgs.haskellPackages.hpc-codecov pkgs.jq];
          coverage = hs-spanshot-coverage;
        } ''
          # Find the mix directory containing the library module coverage data
          # The tix file references modules as "<package-id>/Module" so we need the
          # parent directory that contains the package-id subdirectory
          # Path structure: lib/.../extra-compilation-artifacts/hpc/vanilla/mix/
          MIX_DIR=$(find $coverage/lib -type d -name "mix" -path "*/hpc/vanilla/*" | head -1)

          # Prefer unit test tix (has library coverage) over integration-cli tix
          TIX_FILE=$(find $coverage/share/hpc -name "hs-spanshot-test.tix" 2>/dev/null | head -1)
          if [ -z "$TIX_FILE" ]; then
            TIX_FILE=$(find $coverage/share/hpc -name "*.tix" | head -1)
          fi

          echo "Using MIX_DIR: $MIX_DIR"
          echo "Using TIX_FILE: $TIX_FILE"

          mkdir -p $out

          # Auto-generate exclude list from test modules (any .hs file in test/)
          # This avoids manually maintaining the list when adding new test files
          TEST_MODULES=$(find ${../hs-spanshot}/test -name "*.hs" -exec basename {} .hs \; | tr '\n' ',' | sed 's/,$//')
          EXCLUDES="Main,Paths_hs_spanshot,$TEST_MODULES"

          # Generate report with nix store paths
          hpc-codecov \
            --mix="$MIX_DIR" \
            --src=${../hs-spanshot} \
            --exclude="$EXCLUDES" \
            --out=$out/codecov-raw.json \
            "$TIX_FILE"

          # Rewrite nix store paths to relative repository paths
          # /nix/store/xxx-hs-spanshot/src/File.hs -> hs-spanshot/src/File.hs
          jq '.coverage | to_entries | map({key: (.key | sub("/nix/store/[^/]+-hs-spanshot/"; "hs-spanshot/")), value: .value}) | from_entries | {coverage: .}' \
            $out/codecov-raw.json > $out/codecov.json
          rm $out/codecov-raw.json
        '';
    in {
      hs-spanshot = hs-spanshot-with-alias;
      default = hs-spanshot-with-alias;
      inherit hs-spanshot-coverage hs-spanshot-codecov-report;
    };
  };
}
