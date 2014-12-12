# This is currently a Nix expression for building with cabal but I
# plan on changing it so it just builds with GNU Make.  That way all
# of my sandboxing will work correctly.
{ pkgs ? (import <nixpkgs> {}) }:

let haskellPackages = pkgs.haskellPackages_ghc783_profiling; in

haskellPackages.cabal.mkDerivation (self: {
  pname = "edify";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;

  buildTools = with pkgs; [
    haskellPackages.ghc
    haskellPackages.cabalInstall
  ];

  buildDepends = with pkgs; [
    # Haskell packages.
    haskellPackages.mtl

    # Libraries needed by Haskell packages:
    zlib gcc
  ];

  meta = {
    homepage = "http://www.pmade.com";
    description = "Markdown (via pandoc) processing and content tools for Devalot.com";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
