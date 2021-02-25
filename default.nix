let sources = import nix/sources.nix;
in
{ nixpkgs ? "nixpkgs" # vs. nixpkgs-unstable
, pkgs ? import sources.${nixpkgs} { }
, ghc ? "default" # or 8.10.4
}:
let
  nix-hs = import sources.nix-hs { inherit pkgs; };
  deps = import nix/deps.nix { inherit pkgs; };
  path = pkgs.lib.makeBinPath deps;


  drv = nix-hs {
    cabal = ./edify.cabal;
    compiler = ghc;

    overrides = lib: self: super: {
      aeson = super.aeson_1_5_2_0;

      optparse-applicative =
        if super ? optparse-applicative_0_16_0_0
        then super.optparse-applicative_0_16_0_0
        else super.optparse-applicative;
    };
  };

in
drv.overrideAttrs (orig: {
  buildInputs =
    (orig.buildInputs or [ ])
    ++ [ pkgs.makeWrapper ];

  # Wrap the edify executable so it can access all of the run-time
  # dependencies:
  postInstall =
    (orig.postInstall or "")
    + ''
      mkdir -p "$out/wrapped"
      mv "$out/bin/edify" "$out/wrapped/edify"

      makeWrapper "$out/wrapped/edify" "$out/bin/edify" \
        --prefix PATH : "${path}"
    '';
})
