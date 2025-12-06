{inputs, ...}: {
  imports = [
    inputs.agenix-shell.flakeModules.default
  ];
  agenix-shell = {
    secrets = {
      CACHIX_AUTH_TOKEN.file = ./CACHIX_AUTH_TOKEN.age;
      CODECOV_TOKEN.file = ./CODECOV_TOKEN.age;
      PAT_CODEX_SPANSHOT_WRITE.file = ./PAT_CODEX_SPANSHOT_WRITE.age;
    };
  };
}
