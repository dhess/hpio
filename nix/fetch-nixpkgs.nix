let

  lib = import ./lib.nix;
  nixpkgs = lib.fetchNixPkgs;

in nixpkgs
