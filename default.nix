{ sources ? import ./nix/sources.nix, nixpkgs ? "nixpkgs"
, pkgs ? import sources.${nixpkgs} { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }, ghc ? "default" }:

nix-hs {
  cabal = ./edify.cabal;
  compiler = if ghc == "default" then "ghc-8.8.3" else ghc;

  overrides = lib: self: super: {
    relude =
      if super ? relude_0_6_0_0 then super.relude_0_6_0_0 else super.relude;
  };
}
