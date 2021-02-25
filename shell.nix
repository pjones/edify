# Load an interactive environment:
let
  sources = import nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  deps = import nix/deps.nix { inherit pkgs; };
in
(import ./. {
  inherit pkgs;
}).interactive.overrideAttrs (orig: {
  buildInputs = orig.buildInputs ++ deps;
})
