{ pkgs ? (import <nixpkgs> {}).pkgs }:
pkgs.haskellPackages.callPackage ./edify.nix { }
