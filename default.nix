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
    byline = super.callCabal2nix "byline" "${sources.byline}/mtl" { };
    generic-lens = super.callHackage "generic-lens" "2.0.0.0" { };
    commonmark = super.callCabal2nix "commonmark" "${sources.commonmark-hs}/commonmark" { };

    haskeline =
      if super ? haskeline_0_8_0_0 then
        lib.dontCheck super.haskeline_0_8_0_0
      else
        super.haskeline;
  };
}
