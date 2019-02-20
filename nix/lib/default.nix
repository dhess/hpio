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
  inherit (dhess-lib-nix) lib;
  inherit (lib.fetchers) fixedNixpkgs;
  inherit (lib.dhess-lib-nix) nixpkgs;

  overlays = [
    dhess-lib-nix.overlays.all
    (import ../overlays/haskell-overrides.nix)
  ];

  maintainerOverlays = [
    dhess-lib-nix.overlays.all
    (import ../overlays/haskell-overrides-maintainer.nix)
  ];

in lib //
{
  inherit fixedNixpkgs;
  inherit nixpkgs;
  inherit overlays maintainerOverlays;
}
