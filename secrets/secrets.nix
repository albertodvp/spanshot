let
  users = builtins.fromJSON (builtins.readFile ../users.json);
  publicKeys =
    builtins.foldl'
    (acc: user:
      acc
      // {
        "${user.username}" = user.publicKey;
      })
    {}
    users;
in
  with publicKeys; {
    "CACHIX_AUTH_TOKEN.age".publicKeys = [albertodvp gha];
    "CODECOV_TOKEN.age".publicKeys = [albertodvp gha];
    "PAT_CODEX_SPANSHOT_WRITE.age".publicKeys = [albertodvp gha];
  }
