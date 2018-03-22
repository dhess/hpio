let

  lib = import ./lib.nix;
  nixpkgs = import lib.fetchNixPkgs { config = {}; };

in nixpkgs
