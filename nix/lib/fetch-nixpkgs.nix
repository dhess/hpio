let

  lib = import ./.;
  pkgs = lib.nixpkgs { config = {}; };

in pkgs
