# These arguments are so you can override settings from the command
# line using the `nix-hs' tool.
{ nixpkgs   ? import <nixpkgs> { }
, compiler  ? "default"
, profiling ? false
}:

nixpkgs.nix-hs.interactive ./edify.nix {
  inherit compiler profiling;

  # Extra dependencies:
  buildInputs = with nixpkgs; [
  ];
}
