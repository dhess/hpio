let

  # From https://github.com/input-output-hk/iohk-ops/blob/e6f1ae95cdbfdd5c213aa0b9a1ef67150febc503/lib.nix
  
  fixedDhessLibNix =
  let
    try = builtins.tryEval <dhess_lib_nix>;
  in
    if try.success
      then builtins.trace "Using <dhess_lib_nix>" try.value
      else (import ./fetch-github.nix) { jsonSpec = builtins.readFile ./dhess-lib-nix-src.json; };

  dhess-lib-nix = (import fixedDhessLibNix) {};
  inherit (dhess-lib-nix.lib.fetchers) fixedNixpkgs;
  inherit (dhess-lib-nix.lib.dhess-lib-nix) nixpkgs;

in
{
  inherit fixedNixpkgs;
  inherit dhess-lib-nix;
  inherit nixpkgs;
}
