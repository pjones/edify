with (import <nixpkgs> {});

stdenv.mkDerivation {
  name = "edify";

  buildInputs = [
    # GHC:
    haskell.packages.lts-4_2.ghc

    # Non-Haskell Dependencies:
    zlib
  ];
}
