with (import <nixpkgs> {});

stdenv.mkDerivation {
  name = "edify";

  buildInputs = [
    # GHC:
    haskell.packages.lts-5_15.ghc

    # Non-Haskell Dependencies:
    zlib.out
  ];

  # Work around a bug in GHC:
  # https://ghc.haskell.org/trac/ghc/ticket/11042
  shellHook = ''
    export LD_LIBRARY_PATH=${zlib.out}/lib
  '';
}
