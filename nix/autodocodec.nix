# autodocodec 0.5.0.0 from Hackage
#
# Why we need this:
# nixpkgs GHC 9.12 package set may not have autodocodec, or has an incompatible version.
# This package is required by opt-env-conf and also used directly by hs-spanshot
# for configuration parsing with HasCodec instances.
#
# To regenerate:
#   nix-shell -p cabal2nix cabal-install --run "cabal update && cabal2nix cabal://autodocodec-0.5.0.0"
#
{
  mkDerivation,
  aeson,
  base,
  bytestring,
  containers,
  dlist,
  hashable,
  lib,
  mtl,
  scientific,
  text,
  time,
  unordered-containers,
  validity,
  validity-scientific,
  vector,
}:
mkDerivation {
  pname = "autodocodec";
  version = "0.5.0.0";
  sha256 = "89008a8e6888558f96683de68077cd5f1384d6f7572fcd19b0f2d0ec32095f9c";
  libraryHaskellDepends = [
    aeson
    base
    bytestring
    containers
    dlist
    hashable
    mtl
    scientific
    text
    time
    unordered-containers
    validity
    validity-scientific
    vector
  ];
  # Skip doctests - they often have issues in Nix builds
  doCheck = false;
  homepage = "https://github.com/NorfairKing/autodocodec#readme";
  description = "Self-documenting encoder and decoder";
  license = lib.licenses.mit;
}
