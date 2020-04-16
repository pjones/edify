let
  commit  = "342eaba9ebbc68923c1028a56be1c94e74862832";
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/${commit}";
in import nixpkgs { }
