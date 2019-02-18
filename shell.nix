let

  localLib = import nix/lib.nix;

  overlays = [
    (import nix/overlays.nix)
  ];

  nixpkgs = localLib.nixpkgs;
  pkgs = nixpkgs { inherit overlays; };
  drv = pkgs.haskellPackages.hpio;

in
if pkgs.lib.inNixShell then drv.env else drv
