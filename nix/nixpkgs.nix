# This file loads a specific version nixpkgs and uses the overlay
# system to override it a bit.
let
  commit  = "5b8a24a40ce11bbe1cb6ebf33ddb5adaaebbd43a";
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/${commit}";

in
  import nixpkgs {
    overlays = [
      (import ./overlay.nix)
    ];
  }
