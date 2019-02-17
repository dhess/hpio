let

  localLib = import nix/lib.nix;

  overlays = [
    (import ./.)
    localLib.dhess-lib-nix.overlays.all
  ];

  nixpkgs = localLib.nixpkgs;
  pkgs = nixpkgs { inherit overlays; };
  drv = pkgs.haskellPackages.hpio;

in
if pkgs.lib.inNixShell then drv.env else drv
