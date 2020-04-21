{ pkgs ? import nix/nixpkgs.nix
}:

let
  nix-hs = import (fetchGit {
    url = "https://code.devalot.com/open/nix-hs.git";
    rev = "ab8f15a5a84d0d685c42e8fcfec3cf34755b562f";
    ref = "next";
  }) { inherit pkgs; };

in nix-hs {
  cabal = ./edify.cabal;

  overrides = lib: self: super: with lib; {
    ghcide = (import (builtins.fetchGit {
      url = "https://code.devalot.com/pjones/ghcide-nix.git";
      rev = "471990016e47f6eaab5d6aeeb2da6f58aa581bb7";
    })) {};
  };
}
