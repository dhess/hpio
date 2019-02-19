let

  lib = import ./.;
  pkgs = import lib.nixpkgs { config = {}; };

in pkgs
