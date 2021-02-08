{ sources ? import ./nix/sources.nix
, nixpkgs ? "nixpkgs"
, pkgs ? import sources.${nixpkgs} { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghc ? "default"
}:

nix-hs {
  cabal = ./edify.cabal;
  compiler = ghc;

  overrides = lib: self: super: {
    aeson = super.aeson_1_5_2_0;

    haddock-library =
      lib.doJailbreak super.haddock-library;

    haskeline = lib.dontCheck (
      if super ? haskeline_0_8_1_0
      then lib.dontCheck super.haskeline_0_8_1_0
      else super.haskeline
    );

    optparse-applicative =
      if super ? optparse-applicative_0_16_0_0
      then super.optparse-applicative_0_16_0_0
      else super.optparse-applicative;
  };
}
