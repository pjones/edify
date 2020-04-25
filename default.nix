{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghcide ? sources.ghcide-nix
, ormolu ? sources.ormolu
, ghc ? "default"
}:

nix-hs {
  cabal = ./edify.cabal;
  compiler = ghc;

  overrides = lib: self: super: with lib; {
    relude =
      if super ? relude_0_6_0_0
        then super.relude_0_6_0_0
        else super.relude;

    ghcide = import ghcide {};

    ormolu = (import ormolu {
      inherit (lib) pkgs;
      ormoluCompiler = lib.compilerName;
    }).ormolu;
  };
}
