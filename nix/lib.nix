let

  # From https://github.com/input-output-hk/iohk-ops/blob/e6f1ae95cdbfdd5c213aa0b9a1ef67150febc503/lib.nix
  
  fetchNixPkgs =
  let
    try = builtins.tryEval <nixpkgs_override>;
  in
    if try.success
      then builtins.trace "Using <nixpkgs_override>" try.value
      else import ./fetch-nixpkgs.nix;

  nixpkgs = import fetchNixPkgs;

  pkgs = nixpkgs {};

  lib = pkgs.lib;

in lib // (rec {

  inherit fetchNixPkgs;

})
