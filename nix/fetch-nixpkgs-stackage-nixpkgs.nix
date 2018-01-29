let

  lib = import ./lib.nix;
  nixpkgs = lib.fetchNixPkgsStackageNixPkgs;

in nixpkgs
