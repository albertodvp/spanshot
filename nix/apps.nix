{...}: {
  perSystem = {
    pkgs,
    lib,
    config,
    ...
  }: {
    apps.default = {
      type = "app";
      program = "${config.packages.hs-spanshot}/bin/spanshot";
    };

    # CI app: Configure Cachix with auth token from agenix
    # Usage: nix run .#configure-cachix
    # Requires: SSH key for agenix decryption
    apps.configure-cachix = {
      type = "app";
      program = let
        script = pkgs.writeShellScript "configure-cachix" ''
          set -euo pipefail
          source ${lib.getExe config.agenix-shell.installationScript}

          ${pkgs.cachix}/bin/cachix authtoken "$CACHIX_AUTH_TOKEN"
          ${pkgs.cachix}/bin/cachix use spanshot
        '';
      in "${script}";
    };

    # CI app: Export secrets to GITHUB_ENV for use in subsequent steps
    # Usage: nix run .#export-secrets >> "$GITHUB_ENV"
    # Requires: SSH key for agenix decryption
    apps.export-secrets = {
      type = "app";
      program = let
        script = pkgs.writeShellScript "export-secrets" ''
          set -euo pipefail
          source ${lib.getExe config.agenix-shell.installationScript} 2>/dev/null

          # Output in GITHUB_ENV format, masked for security
          echo "::add-mask::$CODECOV_TOKEN"
          echo "CODECOV_TOKEN=$CODECOV_TOKEN"
        '';
      in "${script}";
    };

    # CI app: Trigger README sync via Codex
    # Usage: nix run .#trigger-readme-sync
    # Requires: SSH key for agenix decryption, GITHUB_REPOSITORY env var
    apps.trigger-readme-sync = {
      type = "app";
      program = let
        script = pkgs.writeShellScript "trigger-readme-sync" ''
          set -euo pipefail
          source ${lib.getExe config.agenix-shell.installationScript}

          export GH_TOKEN="$PAT_CODEX_SPANSHOT_WRITE"
          ${pkgs.gh}/bin/gh issue comment 9 \
            --repo "''${GITHUB_REPOSITORY:-albertodvp/spanshot}" \
            --body "@codex Please sync the README following the instructions in prompts/readme-sync.md. If any changes are needed, open a PR."
        '';
      in "${script}";
    };
  };
}
