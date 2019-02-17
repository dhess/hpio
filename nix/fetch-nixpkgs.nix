let

  lib = import ./lib.nix;
  pkgs = import lib.nixpkgs { config = {}; };

in pkgs
