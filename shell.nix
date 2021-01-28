# Load an interactive environment:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
in
(import ./. {
  inherit pkgs;
}).interactive.overrideAttrs (orig: {
  buildInputs = orig.buildInputs ++ import ./nix/deps.nix {
    inherit pkgs;
  };
})
