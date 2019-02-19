let

  localLib = (import ./nix/lib);
  defaultPkgs = localLib.nixpkgs {};

in

{ pkgs ? defaultPkgs }:

let

  inherit (localLib.dhess-lib-nix) lib;

  hpioOverlays = import ./nix/overlays.nix;
  hpioNix = nix/pkgs/hpio.nix;
  hpioPkgs = lib.customisation.composeOverlays (lib.singleton hpioOverlays) pkgs;

in
{
  # haskellPackages with the local hpio package.
  inherit (hpioPkgs) haskellPackages;

  # The path to the local hpio.nix, in case you want to make your own.
  inherit hpioNix;

  overlays.hpio = hpioOverlays;
}
