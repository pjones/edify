with (import <nixpkgs> {});

stdenv.mkDerivation {
  name = "edify";

  buildInputs = [
    # GHC:
    haskell.packages.lts-4_2.ghc

    # Non-Haskell Dependencies:
    zlib
  ];

    # Work around a bug in GHC:
  # https://ghc.haskell.org/trac/ghc/ticket/11042
  shellHook = ''
    export LD_LIBRARY_PATH=${zlib}/lib
  '';
}
